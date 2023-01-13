package o1.adventure

import scala.collection.mutable

class Enemy(val name: String, val hitPoints: Int, val belongings: mutable.Buffer[Item]) {

  override def toString = this.name

  }
