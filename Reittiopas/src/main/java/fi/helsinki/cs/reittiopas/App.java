package fi.helsinki.cs.reittiopas;

public class App {

    /**
     * Toteuta luokka Reittiopas siten, että haettaessa reittiä kahden pysäkin
     * väliltä se palauttaa valmiin reitin Tila-olioina taaksepäin linkitettynä
     * listana siten, että palautettu "Tila" osoittaa maalipysäkkiin.
     * Esimerkiksi kutsuttaessa reittiopasta lähtöpysäkillä
     * "1250429(Metsolantie)" ja maalipysäkillä "1121480(Urheilutalo)" saadaan
     * palautuksena Tila-olio, jonka pysäkki on "1121480(Urheilutalo)" ja
     * "edellinen" osoittaa toiseksi viimeiseen pysäkkiin jne.
     *
     * Kelattaessa palautuksena saatu linkitetty lista lopusta alkuun saadaan
     * kokonaisuudessaan seuraava reitti:
     *
     * 1121480(Urheilutalo)[MAALI] -> 1121438(Brahenkatu) -> 1220414(Roineentie)
     * -> 1220416(Hattulantie) -> 1220418(Rautalammintie) ->
     * 1220420(Mäkelänrinne) -> 1220426(Uintikeskus) -> 1173416(Pyöräilystadion)
     * -> 1173423(Koskelantie) -> 1250425(Kimmontie) -> 1250427(Käpylänaukio) ->
     * 1250429(Metsolantie)[LÄHTÖ]
     *
     * Linkitetyn listan viimeisen Tilan pysäkki on lähtöpysäkki "1250429" ja
     * "edellinen" on null.
     *
     * Projekti sisältää JUnit-yksikkötestit tälle esimerkkireitille, mutta
     * testit eivät testaa muita reittejä.
     *
     * @param args args
     */
    public static void main(String[] args) {

        Pysakkiverkko pysakkiverkko = new Pysakkiverkko("verkko.json");
        Reittiopas reittiopas = new Reittiopas();
        Pysakki lahto = pysakkiverkko.getPysakki("1250429");
        Pysakki maali = pysakkiverkko.getPysakki("1121480");
        Tila tila = reittiopas.haku(lahto, maali);
        while (tila != null) {
            System.out.println(tila.getPysakki());
            tila = tila.getEdellinen();
        }
    }
}
