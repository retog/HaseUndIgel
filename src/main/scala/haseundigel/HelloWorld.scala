package haseundigel

object HelloWorld {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
	/*for (i <- 1 to 50) {
	  zeigeKosten(i)
	}*/
	zeigeSprünge(14,56)
  }
  
  def zeigeKosten(felder: Int) {
	println("Kosten für "+felder+": "+kosten(felder))
  }
  
  def zeigeSprünge(felder: Int, budget: Int) {
	val sprungListe = wieSpringeIch(felder, budget)
	println("Um "+felder+" Felder mit höchstens "+budget+" Karotten weiter zu kommen hopsen Sie "+sprungListe.size+" mal wie folgt: zuerst "+sprungListe.mkString(" und dann "))
	var total = 0
	for (sprung <- sprungListe) {
	  zeigeKosten(sprung)
	  total = total + kosten(sprung)
	}
	println("Gesamtkosten: "+total)
	println("Es bleiben für Sie "+(budget-total)+" Karotten übrig")
  }
  
  def wieSpringeIch(felder: Int, budget: Int): List[Int] = {
	if (felder > budget) {
	  throw new RuntimeException("das geht nicht einmal im igel tempo!")
	}
	for(anzahlSprünge <- 1 to felder) {
	  val kurzSprung = felder / anzahlSprünge
	  val anzahlLangSprünge = felder % anzahlSprünge
	  val anzahlKurzSprünge = anzahlSprünge - anzahlLangSprünge
	  var gesamtKosten = anzahlKurzSprünge * kosten(kurzSprung)
	  gesamtKosten = gesamtKosten + anzahlLangSprünge * kosten(kurzSprung + 1)
	  if (gesamtKosten <= budget) {
		
		return (for (i <- 1 to anzahlKurzSprünge) yield kurzSprung).toList ::: 
		  (for (i <- 1 to anzahlLangSprünge) yield (kurzSprung+1)).toList
	  }
	}
	throw new RuntimeException("etwas ging gründlich schief")
  }
  
  def kosten(felder: Int): Int = {
	felder match {
	  case 1 => 1
	  case n => kosten(n-1)+n
	}
  } 
}
 