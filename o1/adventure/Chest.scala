package o1.adventure

class Chest(var contents: Vector[Item] ) {
  private var locked = true

  def open = {
    var show = contents.headOption
    if(opened) {
  } else {
     locked = false
     var show = contents.headOption
     contents = Vector[Item]()
   }
    show
  }

  def opened = !locked

}
