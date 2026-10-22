declare name    "Vocoder de cuerdas v2";
declare version "0.2";
declare description "La voz (IN L) excita 6 cuerdas Karplus-Strong y esculpe su espectro con un vocoder de 8 bandas.";

import("stdfaust.lib");

//==================================================================
// CONTROLES FISICOS
//==================================================================
// A, B y C NO se suavizan aca a proposito: todo lo que depende solo de ellos
// (pow, exp, log10, sqrt, divisiones) Faust lo calcula una vez por bloque, no por sample.
// El suavizado se aplica despues, sobre los resultados (ver string()).
kA  = hslider("Base[OWL:A]",       0.30, 0, 1, 0.001);   // frecuencia base f0
kB  = hslider("Dispersion[OWL:B]", 0.82, 0, 1, 0.001);   // unisono -> armonico -> inarmonico
kC  = hslider("Decay[OWL:C]",      0.50, 0, 1, 0.001);   // T60 de las cuerdas + release del vocoder
kD  = hslider("Brillo[OWL:D]",     0.50, 0, 1, 0.001) : si.smoo;  // cuerdas opacas <-> brillantes
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
RELX  = 2;       // release del vocoder = RELX veces el T60 de las cuerdas
CURVA = 9;       // curvatura logaritmica de C (mas grande = mas curva; cerca de 0 = recta)
WET   = 32;      // ganancia de salida del vocoder (v1: 8, o sea +12 dB)
NOISE = 0.08;    // nivel del ruido que "frota" las cuerdas en freeze

expmap(lo, hi, k) = lo * pow(hi / lo, k);
logmap(lo, hi, k) = lo + (hi - lo) * log10(1 + CURVA * k) / log10(1 + CURVA);

//==================================================================
// BANCO DE CUERDAS
//==================================================================
f0  = expmap(30, 300, kA);          // 30 -> 300 Hz, continuo
s   = 1.5 * kB * kB;                // 0 -> 1.5 (curva cuadratica: mas resolucion cerca del unisono)
t60 = logmap(0.2, 20, kC);          // 0.2 s -> 20 s, curva logaritmica (mitad de C ~ 15 s)

// Brillo: coeficiente del filtro de perdidas del lazo, (1 - b) + b z^-1
// D = 0 -> b = 0.45 (muy opaco), D = 0.5 -> b ~ 0.24 (como v1), D = 1 -> b = 0.02 (casi sin perdida)
b    = 0.45 - 0.43 * kD;
loss = _ <: *(1 - b), (mem : *(b)) :> _;

// Cuerda i: f_i = f0 * (i+1)^s
// g   = ganancia por vuelta para que la cuerda decaiga 60 dB en t60 segundos
// nrm = sqrt(1-g) mantiene parejo el volumen con decaimientos cortos y largos
// d   = largo del delay; se resta 1 por el ~ y b por el retardo del filtro de perdidas,
//       asi la afinacion no cambia al mover el brillo
string(i, x) = x * nrm : + ~ (de.fdelay4(MAXD, d) * g : loss)
with {
    f   = min(FMAX, f0 * pow(i + 1, s));
    gR  = exp(-6.9078 / (t60 * f));
    g   = gR : si.smoo;
    nrm = sqrt(1 - gR) : si.smoo;
    d   = max(2, ma.SR / f - 1 - b) : si.smoo;
};

strings(x) = par(i, NS, string(i, x)) :> /(3) : fi.dcblocker;

//==================================================================
// VOCODER DE 8 BANDAS
//==================================================================
// Release: T60 del vocoder = RELX * t60. amp_follower_ar usa constante de tiempo,
// que es el T60 dividido 6.9078 (ln 1000).
REL = RELX * t60 / 6.9078;

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
// IN R entra atenuada ~120 dB: no se escucha, pero el compilador online (Faust 2.30.5,
// modo in-place) falla con un ASSERT si alguna entrada de process no se usa.
process(l, r) = outL, outR
with {
    x    = (l + r * 0.000001) : fi.highpass(2, 40);    // IN L = voz
    exc  = x + (frz : si.smoo) * no.noise * NOISE;      // en freeze, el ruido sostiene las cuerdas
    c    = strings(exc);

    wL   = vocL(x, c) * WET;
    wR   = vocR(x, c) * WET;

    // Salidas CV
    envV  = x : an.amp_follower_ar(0.005, 0.1);
    envO  = min(1, envV * 3);
    noisO = min(1, (x : an.zcr(0.02)) * 3) * min(1, envV * 20);   // silenciada si no hay voz

    outL = wL : ma.tanh
         : attach(_, envO : cvEnv) : attach(_, noisO : cvNoise);
    outR = wR : ma.tanh;
};
