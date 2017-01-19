/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fi.helsinki.cs.reittiopas;

import static junit.framework.Assert.*;
import org.junit.*;

/**
 *
 * @author mikko
 */
public class ReittiopasTest {

    private Reittiopas reittiopas;
    private Pysakkiverkko verkko;

    @Before
    public void setUp() {

        verkko = new Pysakkiverkko("verkko.json");
        reittiopas = new Reittiopas();
    }

    @Test
    public void etsiReittiPalauttaaOikein() {
        Pysakki lahto = verkko.getPysakki("1250429");
        Pysakki maali = verkko.getPysakki("1121480");
        Tila tila = reittiopas.haku(lahto, maali);

        String[] oikeaVastaus = {"1121480", "1121438", "1220414", "1220416", "1220418",
            "1220420", "1220426", "1173416", "1173423", "1250425", "1250427", "1250429"};
        
        for (int i = 0; i < oikeaVastaus.length; i++) {
            if (tila == null) {
                fail("Palautuksesi oli liian lyhyt. Palauttamasi listan pituus oli " + i +", lyhimmän mahdollisen reitin pituus on " + oikeaVastaus.length);
            }
            
            if (tila.getPysakki() == null) {
                fail("Palautuksesi sisälsi tilan jossa pysäkki oli null");
            }
            
            assertEquals(oikeaVastaus[i], tila.getPysakki().getKoodi());
            tila = tila.getEdellinen();
        }
        
        assertNull("Palauttamasi reitti oli pidempi kuin 12", tila);

    }

}
