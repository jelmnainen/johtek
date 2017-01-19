package fi.helsinki.cs.reittiopas;

import java.util.Collection;
import java.util.HashMap;
import java.util.Objects;

/**
 * Älä muokkaa tätä luokkaa. Sisältää yksittäisen pysäkin tiedot ja tuntee
 * naapurinsa.
 */
public class Pysakki {

    /**
     * Pysäkin yksiselitteinen koodi. ID, jota voit käyttää pysäkin
     * tunnistamiseen.
     */
    private String koodi;

    /**
     * Pysäkin osoite. Jotkin osoitteet sisältävät kadun ja numeron, toiset vain
     * kadun nimen.
     */
    private String osoite;

    /**
     * Pysäkin nimi. Tämä ei välttämättä ole yksiselitteinen.
     */
    private String nimi;

    /**
     * Pysäkin x-koordinaatti.
     */
    private int x;

    /**
     * Pysäkin y-koordinaatti.
     */
    private int y;

    /**
     * Pysäkin naapuripysäkit ja kaikki ko. pysäkille kulkevat linjat (niiden
     * yksikäsitteiset koodit). key: Pysakki.koodi, value: Linja.koodi-taulukko.
     *
     */
    private HashMap<String, String[]> naapurit;

    private Collection<Pysakki> naapuriPysakit;

    public Pysakki() {
        this.koodi = "";
        this.osoite = "";
        this.nimi = "";
        this.x = 0;
        this.y = 0;
        this.naapurit = new HashMap<String, String[]>();
    }

    public String toString() {
        return this.koodi + "(" + this.nimi + ")";
    }

    public String getKoodi() {
        return koodi;
    }

    public Collection<String> getNaapuriPysakkiKoodit() {
        return this.naapurit.keySet();
    }

    /**
     * Palauttaa tämän pysäkin naapuripysäkit eli pysäkit, joihin tästä
     * pysäkistä on suora yhteys.
     *
     * @return naapurit
     */
    public Collection<Pysakki> getNaapurit() {
        return this.naapuriPysakit;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 97 * hash + Objects.hashCode(this.koodi);
        hash = 97 * hash + Objects.hashCode(this.osoite);
        hash = 97 * hash + Objects.hashCode(this.nimi);
        hash = 97 * hash + this.x;
        hash = 97 * hash + this.y;
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Pysakki other = (Pysakki) obj;
        if (!Objects.equals(this.koodi, other.koodi)) {
            return false;
        }
        if (!Objects.equals(this.osoite, other.osoite)) {
            return false;
        }
        if (!Objects.equals(this.nimi, other.nimi)) {
            return false;
        }
        if (this.x != other.x) {
            return false;
        }
        if (this.y != other.y) {
            return false;
        }
        return true;
    }

    public void setNaapurit(Collection<Pysakki> pysakit) {
        this.naapuriPysakit = pysakit;
    }

}
