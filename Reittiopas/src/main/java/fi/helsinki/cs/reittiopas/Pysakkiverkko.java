package fi.helsinki.cs.reittiopas;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonParser;
import java.util.HashMap;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Luokka pysäkkiverkon kuvaamiseen ja sen tietojen lukemiseen JSON-tiedostosta
 */
public class Pysakkiverkko {

    /**
     * Kaikki pysakit taulukossa.
     *
     * Pari huomioita pysakkien muodostamasta verkosta: 1.) verkko on osittain
     * suunnattu, eli jossain kohdissa pysakkien valin paasee kulkemaan vain
     * toiseen suuntaan.
     *
     */
    private final Pysakki[] pysakit;

    /**
     * Pysakit key: koodi value: olio hakupareina. Oliot ovat samat kuin
     * pysakit-taulukossa.
     *
     */
    private HashMap<String, Pysakki> psMap;

    /**
     * Konstruktori. Lukee verkkoa kuvaavan JSON-tiedoston ja luo sen pohjalta
     * Pysakki-olioiden verkon.
     *
     * @param filePath polku tiedostoon (sis. tiedoston nimen)
     */
    public Pysakkiverkko(String filePath) {
        JsonArray psArr = readJSON(filePath);
        Gson gson = new Gson();
        this.pysakit = new Pysakki[psArr.size()];
        for (int i = 0; i < psArr.size(); i++) {
            this.pysakit[i] = gson.fromJson(psArr.get(i), Pysakki.class);
        }
        psMap = new HashMap<String, Pysakki>();
        for (Pysakki p : pysakit) {
            this.psMap.put(p.getKoodi(), p);
        }

        for (Pysakki p : pysakit) {
            Collection<Pysakki> naapurit = new ArrayList<>();
            for (String s : p.getNaapuriPysakkiKoodit()) {
                naapurit.add(psMap.get(s));
            }
            p.setNaapurit(naapurit);
        }

    }

    /**
     * Palauttaa pysäkkikoodia vastaavan Pysäkki-olion
     *
     * @param koodi pysäkin tunniste
     * @return tunnistetta vastaava pysäkki
     */
    public Pysakki getPysakki(String koodi) {

        return psMap.get(koodi);
    }

    /**
     * Palauttaa verkon pysäkit.
     *
     * @return Pysäkki-oliot
     */
    public Pysakki[] getPysakkilista() {

        return pysakit.clone();
    }

    /**
     * Lukee annetun tiedoston palautettavaan Stringiin
     *
     * @param filePath tiedoston sijainti
     * @return tiedoston sisältö merkkijonona
     */
    private String readFileAsString(String filePath) {

        File tiedosto = new File(this.getClass().getClassLoader().getResource(filePath).getFile());
        byte[] buffer;
        try {
            buffer = Files.readAllBytes(tiedosto.toPath());
        } catch (IOException ex) {
            ex.printStackTrace();
            return null;
        }

        return new String(buffer);
    }

    /**
     * Parsii annetusta tiedostosta JSON-taulukon.
     *
     * @param filePath tiedostopolku luettavaan tiedostoon
     * @return JsonArray, joka edustaa tiedostosta luettuja JSON-olioita.
     */
    private JsonArray readJSON(String filePath) {

        JsonParser parser = new JsonParser();
        String json = "";

        json = readFileAsString(filePath);

        JsonArray arr = parser.parse(json).getAsJsonArray();
        return arr;
    }
}
