declare name    "Vocoder de cuerdas v1";
declare version "0.1";
declare description "La voz (IN L) excita 6 cuerdas Karplus-Strong y esculpe su espectro con un vocoder de 8 bandas.";

import("stdfaust.lib");

//==================================================================
// CONTROLES FISICOS
//==================================================================
// A, B y C NO se suavizan aca a proposito: todo lo que depende solo de ellos
// (pow, exp, sqrt, divisiones) Faust lo calcula una vez por bloque, no por sample.
// El suavizado se aplica despues, sobre los resultados (ver string()).
kA  = hslider("Base[OWL:A]",       0.30, 0, 1, 0.001);   // frecuencia base f0
kB  = hslider("Dispersion[OWL:B]", 0.82, 0, 1, 0.001);   // unisono -> armonico -> inarmonico
kC  = hslider("Decay[OWL:C]",      0.50, 0, 1, 0.001);   // T60 de las cuerdas
kD  = hslider("Mix[OWL:D]",        0.70, 0, 1, 0.001) : si.smoo;  // voz seca <-> cuerdas vocodeadas
frz = checkbox("Freeze[OWL:B1]");                        // toggle; el LED muestra el estado

cvEnv   = hbargraph("Envolvente>[OWL:F]", 0, 1);
cvNoise = hbargraph("Ruidosidad>[OWL:G]", 0, 1);

//==================================================================
// CONSTANTES DE AJUSTE (para tocar mientras se experimenta)
//==================================================================
NS    = 6;       // cantidad de cuerdas
NB    = 8;       // bandas del vocoder (par: impares a L, pares a R)
MAXD  = 2048;    // delay maximo de cada cuerda (~23 Hz a 48 kHz)
FMAX  = 6000;    // techo de frecuencia de las cuerdas
FLO   = 150;     // centro de la banda mas grave del vocoder
FRAT  = 1.65;    // razon entre bandas: 150 Hz ... ~5 kHz
Q     = 2.5;     // Q de los pasabandas
ATT   = 0.005;   // ataque de los seguidores (s)
REL   = 0.08;    // release de los seguidores (s)
WET   = 8;       // ganancia de compensacion del vocoder
NOISE = 0.08;    // nivel del ruido que "frota" las cuerdas en freeze

expmap(lo, hi, k) = lo * pow(hi / lo, k);

//==================================================================
// BANCO DE CUERDAS
//==================================================================
f0  = expmap(30, 600, kA);          // 30 -> 600 Hz, continuo
s   = 1.5 * kB * kB;                // 0 -> 1.5 (curva cuadratica: mas resolucion cerca del unisono)
t60 = expmap(0.2, 20, kC);          // 0.2 s -> 20 s

// Filtro de perdidas del lazo: 0.75 + 0.25 z^-1 (amortigua agudos, retardo ~0.25 sample)
loss = _ <: *(0.75), (mem : *(0.25)) :> _;

// Cuerda i: f_i = f0 * (i+1)^s
// g   = ganancia por vuelta para que la cuerda decaiga 60 dB en t60 segundos
// nrm = sqrt(1-g) mantiene parejo el volumen con decaimientos cortos y largos
string(i, x) = x * nrm : + ~ (de.fdelay4(MAXD, d) * g : loss)
with {
    f   = min(FMAX, f0 * pow(i + 1, s));
    gR  = exp(-6.9078 / (t60 * f));
    g   = gR : si.smoo;
    nrm = sqrt(1 - gR) : si.smoo;
    d   = max(2, ma.SR / f - 1.25) : si.smoo;   // -1 por el ~, -0.25 por el filtro de perdidas
};

strings(x) = par(i, NS, string(i, x)) :> /(3) : fi.dcblocker;

//==================================================================
// VOCODER DE 8 BANDAS
//==================================================================
// Sample and hold: t = 1 deja pasar, t = 0 retiene el ultimo valor
hold(t) = select2(t, _, _) ~ _;

fc(i)  = FLO * pow(FRAT, i);
bp(i)  = fi.resonbp(fc(i), Q, 1);

env(i, x)     = x : bp(i) : an.amp_follower_ar(ATT, REL) : hold(1 - frz);
band(i, x, c) = (c : bp(i)) * env(i, x);

vocL(x, c) = sum(k, NB / 2, band(2 * k,     x, c));   // bandas 1, 3, 5, 7
vocR(x, c) = sum(k, NB / 2, band(2 * k + 1, x, c));   // bandas 2, 4, 6, 8

//==================================================================
// PROCESO
//==================================================================
process(l, r) = outL, outR
with {
    x    = l : fi.highpass(2, 40);                       // IN L = voz; IN R sin uso
    exc  = x + (frz : si.smoo) * no.noise * NOISE;      // en freeze, el ruido sostiene las cuerdas
    c    = strings(exc);

    wL   = vocL(x, c) * WET;
    wR   = vocR(x, c) * WET;

    // Salidas CV
    envV  = x : an.amp_follower_ar(0.005, 0.1);
    envO  = min(1, envV * 3);
    noisO = 0;

    outL = (x * (1 - kD) + wL * kD) : ma.tanh
         : attach(_, envO : cvEnv) : attach(_, noisO : cvNoise);
    outR = (x * (1 - kD) + wR * kD) : ma.tanh;
};
