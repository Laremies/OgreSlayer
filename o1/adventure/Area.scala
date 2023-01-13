package o1.adventure

import scala.collection.mutable.Map

/** The class `Area` represents locations in a text adventure game world. A game world
  * consists of areas. In general, an "area" can be pretty much anything: a room, a building,
  * an acre of forest, or something completely different. What different areas have in
  * common is that players can be located in them and that they can have exits leading to
  * other, neighboring areas. An area also has a name and a description.
  * @param name         the name of the area
  * @param description  a basic description of the area (typically not including information about items) */
class Area(var name: String, var description: String) {

  private val neighbors = Map[String, Area]()
  private val items = Map[String, Item]()
  private val enemies = Map[String, Enemy]()
  private val marketItems = Map[String, (Item, Int)]()
  private val chest = Map[String, Chest]()


  /** Returns the area that can be reached from this area by moving in the given direction. The result
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)


  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area) = {
    this.neighbors += direction -> neighbor
  }


  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling
    * the `setNeighbor` method on each of the given direction--area pairs.
    * @param exits  contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]) = {
    this.neighbors ++= exits
  }


  /** Returns a multi-line description of the area as a player sees it. This includes a basic
    * description of the area as well as information about exits and items. The return
    * value has the form "DESCRIPTION\n\nExits available: DIRECTIONS SEPARATED BY SPACES".
    * The directions are listed in an arbitrary order. */
  def fullDescription = {
    val exitList = "\n\nExits available: " + this.neighbors.keys.mkString(", ")
    val itemList = if (this.items.nonEmpty) "\nYou see here: " + this.items.keys.mkString(", ") else ""
    val enemy = if(this.enemies.nonEmpty) "\nYou encountered " +  this.enemies.keys.mkString(" ") + "!" else ""
    val marketList = if (this.marketItems.nonEmpty) "\nHere for sale: " + this.marketItems.values.map(itm => s"${itm._1.name} for ${itm._2} coins").mkString(", ") else ""
    this.description + itemList + enemy + marketList + exitList
  }

   def addItem(item: Item): Unit = {
     this.items += s"${item.name}" -> item
   }

  def removeItem(itemName: String): Option[Item] = {
    this.items.remove(itemName)
  }

  def contains(itemName: String): Boolean = this.items.contains(itemName)
  def marketContains(itemName: String): Boolean = this.marketItems.contains(itemName)

  def attackble = {
  this.enemies.toMap
  }

  def addEnemy(enemy: Enemy) = this.enemies += (enemy.name -> enemy)

  def removeEnemy(enemyName: String) = this.enemies.remove(enemyName)

  def addMarketItem(item: Item, cost: Int) = {
    this.marketItems += item.name -> (item, cost)
  }

  def removeMarketItem(itemName: String): Option[(Item, Int)] = {
    this.marketItems.remove(itemName)
  }

  def itemCost(marketItemName: String): Int = {
    this.marketItems(marketItemName)._2
  }

  def addChest(name: String, chest: Chest) = this.chest += (name -> chest)

   def chests = chest.toMap


  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)



}
