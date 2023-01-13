package o1.adventure

import scala.collection.mutable.Map
import scala.util.Random


/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private val playerItems = Map[String, Item]()
  private var money = 70
  private var merchantItems = Map[String, Item]("golden key" -> new Item("golden key", "A mysterious key... What might it open?", None), "rusty key" -> new Item("rusty key", "A mysterious key... What might it open?", None))
  private var hitPoints = 1
  private var odds = Vector[Int](-20,-10,-5, -25, 10, -15, -5, 20, -30, -10, 100, -10, -10, -10)



  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) = {
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if (destination.isDefined) "You go " + direction + "." else "You can't go " + direction + "."
  }


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    "You rest for a while. Better get a move on, though."
  }


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }

  def inventory: String = {
    if (this.playerItems.isEmpty) {
      if (this.money == 0) {
        "You are empty-handed."
      } else {
        s"You have no items but you have ${this.money} coins."
      }
    } else {
      if (this.money == 0) {
        s"You are carrying:\n${this.playerItems.keys.mkString("\n")}\n\nBut you have no money."
      } else {
        s"You are carrying:\n${this.playerItems.keys.mkString("\n")}\n\nYou have ${this.money} coins."
      }
    }
  }

  def get(itemName: String): String = {
    if (this.location.contains(itemName)) {
      this.playerItems(itemName) = this.location.removeItem(itemName).get
      this.location.removeItem(itemName)
      s"You pick up the $itemName."
    } else
      s"There is no $itemName here to pick up."
  }

  def drop(itemName: String): String = {
    if (this.playerItems.contains(itemName)) {
      this.location.addItem(this.playerItems(itemName))
      this.playerItems -= itemName
      s"You drop the $itemName."
    } else "You don't have that!"
  }

  def examine(itemName: String): String = {
    if (this.playerItems.contains(itemName)) {
      val description = this.playerItems.get(itemName).map(itm => itm.description)
      s"You look closely at the $itemName.\n${description.get}"
    } else {
      "If you want to examine something, you need to pick it up first."
    }
  }

  def buy(itemName: String): String = {
    if (this.location.marketContains(itemName)) {
      if (this.money >= this.location.itemCost(itemName)) {
        this.money -= this.location.itemCost(itemName)
        this.playerItems(itemName) = this.location.removeMarketItem(itemName).map(pair => pair._1).get
        this.location.removeMarketItem(itemName)
        s"You have succesfully purchased the $itemName."
      } else {
        "You don't have enough money for that."
      }
    } else {
      s"There is no $itemName here to buy."
    }
  }

  def has(itemName: String): Boolean = this.playerItems.contains(itemName)

  def attackWith(weapon: String) = {
    if(this.location.attackble.nonEmpty) {
      val enemy = this.location.attackble.head._2
       if(has(weapon)) {
         playerItems(weapon).damage match {
        case Some(damage) =>
           if(damage >= enemy.hitPoints) {
              this.location.removeEnemy(enemy.name)
               if(enemy.belongings.nonEmpty) {
                this.location.addItem(enemy.belongings.head)
                "You have succesfully killed the " + enemy.name + ".\nHe has dropped his " + enemy.belongings.head.name + "."
               } else "You have succesfully killed the " + enemy.name + "."
           } else {
           hitPoints = 0
          "Your sword was not strong enough.\nYou were killed by the " + enemy + "." }
        case None =>
             hitPoints = 0
             "Can't atack with that.\nYou were killed by the " + enemy + "."
         }
       } else "You don't have that."
     } else "There is no one to attack here."
  }

  def killed: Boolean = if(hitPoints == 0) true else false

  def give(itemName: String): String ={
    if (this.merchantItems.nonEmpty) {
      this.location.addItem(this.merchantItems(itemName))
      this.merchantItems -= itemName
      s"The merchant throws a $itemName at you."
    } else {
      "The merchant has nothing for you."
    }
  }

  def greet() = {
    if (this.location.name == "Mountain road") {
      if (this.merchantItems.contains("golden key")) {
        println("You wave to the merchant.")
        give("golden key")
      } else {
        "The merchant nods to you."
      }
    } else {
      "There is no one to greet here."
    }
  }

  def rob() = {
    if (this.location.name == "Mountain road") {
      if (this.merchantItems.contains("rusty key")) {
        println("You try to rob the merchant.")
        println(give("rusty key"))
        "It hits you in the eye and you can't be bothered anymore."
      } else {
        "The two of you have a fistfight and you lose.\nWhat a shame."
      }
    } else {
      "There is no one to rob here."
    }
  }

  def yes() = {
    if(this.location.name == "Tavern") {
      this.money = 0
      "Your drink was spiked and you passed out.\nYou've lost all your money."
    } else {
      "Who are you answering to? Have you gone mad?"
    }
  }

  def no() = {
    if(this.location.name == "Tavern") {
      this.go("to market")
      "You've been kicked out of the Tavern for not bying anything."
    } else {
      "Who are you answering to? Have you gone mad?"
    }
  }

  def infiniteMoney() = {
    this.money = 100000
    "You have so much money that it seems you should be the king..."
  }

  def goblin() = {
    this.money += 10
    println("You got 10 coins.")
  }

  def open(chestName: String): String = {
    if(this.playerItems.contains(("rusty key"))) {
      this.hitPoints = 0
      "The rusty key opens the chest's poison compartment.\nYou die due to poisoning."
    } else if(this.playerItems.contains("golden key")) {
       if(!currentLocation.chests(chestName).opened) {
          val inside = currentLocation.chests(chestName).open
       this.location.addItem(inside.get)
       "You found the " + inside.get.name + ".\nLuckily you didn't have the rusty key."
       } else { "You have opened it already."
       }
    } else {
      "You can't open this."
    }
  }

  def gamble() = {
    if(this.location.name == "Tavern") {
      if (this.money <= 0) {
        this.go("to market")
        "You've been kicked out of the Tavern for not having any money.\nPeasant."
      } else {
      val winnings = odds(Random.nextInt(odds.size))
       money += winnings
      if(winnings > 0) {
        "You have won " + winnings.abs + " coins."
      } else {
        "You have lost " + winnings.abs + " coins."
      }
    }} else {
      "Can't gamble here."
    }
  }

  def help = {
    "For the most enjoyable playing experience please launch the game in AdventureTextUI.\n\n" +
      "\"go (uloskäynti)\" = liikuttaa pelaajan uloskäynnin suunnassa olevalle alueelle\n" +
      "\"rest\" = palauttaa vain tekstin, että ritari lepää hetken (ei vaikuta mihinkään)\n" +
      "\"beer\" = palauttaa vain tekstin ritarin nykyisestä tunnetilasta (ei vaikuta mihinkään)\n" +
      "\"quit\" = lopettaa pelin\n" +
      "\"inventory\" = näyttää mitä esineitä ja paljonko rahaa ritarilla on\n" +
      "\"get (esineen nimi)\" = nostaa esineen maasta ja lisää sen inventoryyn\n" +
      "\"drop (esineen nimi)\" = pudottaa esineen maahan ja poistaa sen inventorystä\n" +
      "\"examine (esineen nimi)\" = antaa lyhyen kuvauksen esineestä, jos se löytyy inventorystä\n" +
      "\"use (esineen nimi)\" = jos alueella on vihollinen, voit yrittää tappaa sen tietyllä esineellä tällä komennolla\n" +
      "\"buy (esineen nimi)\" = tällä komennolla voit ostaa Market Squarelta myynnissä olevia esineitä\n" +
      "\"greet\" = tervehdit Mountain Roadilla sijaitsevaa kauppiasta ja hän pudottaa maahan kultaisen avaimen\n" +
      "\"rob\" = yrität ryöstää kauppiaan ja hän pudottaa maahan ruosteisen avaimen\n" +
      "\"yes\" = suostut ostamaan juoman tavernassa\n" +
      "\"no\" = kieltäydyt ostamasta juoman tavernassa\n" +
      "\"gamble\" = tavernassa komento arpoo voititko vai hävisitkö rahaa ja kuinka paljon\n" +
      "\"open chest\" = avaa vuoren huipulla sijaitsevan arkun, jos sinulta löytyy avain\n" +
      "\"stonks\" = cheat code, joka antaa järjettömän määrän rahaa\n" +
      "\"help\" = listaa komennot ja ohjeistaa lukemaan walkthrough.pdf -tiedoston\n\n" +
    "For all the commands, area information and how to speedrun the game please read the walkthrough.pdf which is in this module's root directory."
  }

  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

}


