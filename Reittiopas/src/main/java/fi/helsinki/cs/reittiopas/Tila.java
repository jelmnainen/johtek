
package fi.helsinki.cs.reittiopas;

public class Tila {

    /**
     * Tila, josta tähän tilaan päädyttiin.
     */
    private Tila edellinen;
    private Pysakki pysakki;


    public Tila(Pysakki pysakki, Tila edellinen) {
        this.pysakki = pysakki;
        this.edellinen = edellinen;
    }

    public Tila getEdellinen() {
        return edellinen;
    }

    public Pysakki getPysakki() {
        return pysakki;
    }
}
