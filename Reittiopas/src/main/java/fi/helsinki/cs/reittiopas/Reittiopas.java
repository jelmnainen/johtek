package fi.helsinki.cs.reittiopas;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

public class Reittiopas {

    /**
     * Toteuta leveyssuuntainen haku. Palauta reitti taaksepäin linkitettynä
     * listana Tila-olioita, joista ensimmäinen osoittaa maalipysäkkiin ja
     * jokainen tuntee pysäkin ja tilan, josta kyseiseen tilaan päästiin
     * (viimeisen solmun Pysäkki on lähtöpysäkki ja edellinen tila on null).
     *
     * Voit selvittää pysäkin naapuripysäkit, eli pysäkit joihin pysäkiltä on
     * suora yhteys, kutsumalla pysäkin getNaapurit() -metodia.
     *
     * @param lahto Lahtopysakin koodi
     * @param maali Maalipysakin koodi
     * @return Tila-olioista koostuva linkitetty lista maalista lähtötilaan
     */
    public Tila haku(Pysakki lahto, Pysakki maali) {
        ArrayDeque<Tila> solmulista = new ArrayDeque();
        HashSet<Pysakki> kasitellyt = new HashSet();
        
        solmulista.add(new Tila(lahto, null));
        while(!solmulista.isEmpty()) {
            Tila solmu = solmulista.poll();
            Pysakki solmunPysakki = solmu.getPysakki();
            kasitellyt.add(solmunPysakki);
            if (solmunPysakki.equals(maali)) {
                return solmu;
            }
            solmulista = lisaa(solmunPysakki.getNaapurit(), solmulista, solmu, kasitellyt);    
        }
        
        return null;
    }

    private ArrayDeque<Tila> lisaa(Collection<Pysakki> naapurit, ArrayDeque<Tila> solmulista, Tila edellinen, HashSet<Pysakki> kasitellyt) {
        for (Pysakki pysakki : naapurit) {
            if (!kasitellyt.contains(pysakki)) {
                solmulista.add(new Tila(pysakki, edellinen));
            }
        }
        return solmulista;
    }

}
