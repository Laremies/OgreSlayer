package o1.adventure

import scala.collection.mutable


/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game. */
class Adventure {

  /** The title of the adventure game. */
  val title = "Ogre Slayer"

  private val castle       = new Area("Castle", "Castle seems busy, but you need to be on your way to slay the mighty ogre.\nDon't forget your sword. The King trusts you.")
  private val marketSquare = new Area("Market Square", "Whatever you will need, you will most likely find it here.")
  private val path         = new Area("Path", "Path that goes towards the Dark Forest.\nBe careful as it seems to have rained the day before at the swamp.")
  private val swamp        = new Area("The Swamp", "After rain it is impossible to walk through without good equipment.")
  private val darkForest   = new Area("The Dark Forest", "All kinds of scary monsters live here.")
  private val tavern       = new Area("Tavern", "Welcome to the tavern! Would you like something to drink? You can also gamble with your money here.")
  private val road         = new Area("Mountain road", "A steep road towards the mountain.\nYou see a friendly merchant. Do you walk past him, rob him or greet him?")
  private val mountainBase = new Area("Base of the mountain", "You finally reached the base of the mountain.\nIt starts to get freezing if you go further up.")
  private val summit       = new Area("Summit", "You're at the summit. You see a mysterious chest next to the cave.")
  private val cave         = new Area("Cave", "You hear a woman scream.")
  private val camp         = new Area("Goblin camp", "Many goblins live here peacefully.\nThey tend to steal coins.")


       castle.setNeighbors(Vector( "through the gate" -> marketSquare                                                                                     ))
 marketSquare.setNeighbors(Vector(    "to the castle" -> castle,                  "east" -> road,        "to the tavern" -> tavern,     "west" -> path    ))
       tavern.setNeighbors(Vector(        "to market" -> marketSquare                                                                                     ))
         path.setNeighbors(Vector(             "east" -> marketSquare,           "south" -> swamp,                "west" -> camp                          ))
        swamp.setNeighbors(Vector(            "north" -> path,                   "south" -> darkForest                                                    ))
   darkForest.setNeighbors(Vector(            "north" -> swamp,                   "east" -> mountainBase,        "south" -> darkForest                    ))
         road.setNeighbors(Vector(            "south" -> mountainBase,            "west" -> marketSquare                                                  ))
 mountainBase.setNeighbors(Vector(            "north" -> road,                      "up" -> summit,               "west" -> darkForest                    ))
       summit.setNeighbors(Vector(            "down"  -> mountainBase,  "in to the cave" -> cave                                                          ))
         cave.setNeighbors(Vector(  "out of the cave" -> summit                                                                                           ))
         camp.setNeighbors(Vector(             "east" -> path                                                                                             ))



  private val wSword = new Item("wooden sword", "Effective only against the weakest of enemies.", Some(1))
  private val mSword = new Item("metal sword", "Effective against some of the enemies.", Some(2))
  private val fur    = new Item("fur", "It can help you repel the cold.", None)

  this.castle.addItem(wSword)

  this.marketSquare.addMarketItem(new Item("wellies", "Good in wet conditions.", None), 20)
  this.marketSquare.addMarketItem(new Item("lantern", "Helps you see in the dark.", None), 40)
  this.marketSquare.addMarketItem(new Item("bath duck", "Makes baths even more relaxing.", None), 5)
  this.marketSquare.addMarketItem(new Item("roses", "A batch of red roses. Useful for seducing princesses.", None), 10)
  this.marketSquare.addMarketItem(new Item("silk cape", "Makes you look fabulous even on the battlefield.", None), 70)

  private val chest = new Chest(Vector(new Item("holy sword", "Can kill literally anything", Some(4))))
  summit.addChest("chest", chest)

  private val goblin       = new Enemy("angry goblin", 1, mutable.Buffer(mSword))
  private val normalGoblin = new Enemy("goblin", 1, mutable.Buffer())
  private val mightyOgre   = new Enemy("mighty ogre", 3, mutable.Buffer())
  private val ogre         = new Enemy("ogre", 2, mutable.Buffer(fur))

  path.addEnemy(goblin)
  cave.addEnemy(mightyOgre)
  darkForest.addEnemy(ogre)
  camp.addEnemy(normalGoblin)


  /** The character that the player controls in the game. */
  val player = new Player(castle)


  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 999

  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.cave.attackble.isEmpty


  def check = {
    if(camp.attackble.isEmpty) {
      camp.addEnemy(normalGoblin)
      this.player.goblin()
      false
    } else false
  }

  /** Checks if the player has gone to the swamp without the proper item */
  def drowned = (this.player.location == swamp && !this.player.has("wellies"))

  /** Checks if the player has gone to the mountain without the proper item */
  def freezed = (this.player.location == summit && !this.player.has("fur"))

  /** Checks if the player has gone to the mountain without the proper item */
  def blind = (this.player.location == cave && !this.player.has("lantern"))

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit || this.player.killed || this.drowned || this.freezed || this.blind || this.check

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "\nPlease launch the game in AdventureTextUI for the most enjoyable playing experience.\n\nA mighty Ogre has kidnapped the princess.\nThe King needs you to find and kill the Ogre to get her back. But be careful as the way for the Ogre's lair is hard and you may need some equipment to deal with the obstacles..."


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "You have saved the princess.\nThe King is eternally grateful to you. Hopefully nothing else will happen in the future..."
    else if (this.turnCount == this.timeLimit)
      "Oh no! Time's up. Ogre has had enough time to cook his dinner.\nGame over!"
    else if (drowned)
      "You didn't have the proper equipment.\nYou drowned.\nGame over!"
    else if (freezed)
      "You didn't have the proper equipment.\nYou froze to death.\nGame over!"
    else if (blind)
      "You didn't see anything and the ogre attacked you.\nYou were slashed to death.\nGame over!"
    else  // game over due to player quitting
      "You couldn't save the princess :(.\nGame Over!"
  }


  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined) {
      this.turnCount += 1
    }
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
  }


}

