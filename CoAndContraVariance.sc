class VendingMachine[+A](val currentItem: Option[A], items: List[A]) {

  def this(items: List[A]) = this(None, items)

  def dispenseNext(): VendingMachine[A] = {
    items match {
      case Nil => {
        if (currentItem.isDefined)
          new VendingMachine(None, Nil)
        else
          this
      }
      case x :: xs => new VendingMachine(Some(x), xs)
    }
  }

  def addAll[B >: A](newItems: List[B]): VendingMachine[B] = {
    new VendingMachine(items ++ newItems)
  }

}

class Drink {}

class SoftDrink extends {}

class Cola extends SoftDrink {}

class TonicWater extends SoftDrink {}

def install(vendingMachine: VendingMachine[SoftDrink]): Unit = {
}

//covariant
install(new VendingMachine[Cola](List.empty[Cola]))
install(new VendingMachine[TonicWater](List.empty[TonicWater]))
//invariant
install(new VendingMachine[SoftDrink](List.empty[SoftDrink]))
//install(new VendingMachine[Drink])
//VendingMachine[A] :< VendingMachine[B] if A :< B
val colasVM: VendingMachine[Cola] = new VendingMachine(List(new Cola, new Cola))
val softDrinksVM: VendingMachine[SoftDrink] = colasVM.addAll(List(new TonicWater))

//A Good example
trait Bullet

class NormalBullet extends Bullet

class ExplosiveBullet extends Bullet

final class AmmoMagazine[+A <: Bullet](private[this] var bullets: List[A]) {

  def hasBullets: Boolean = !bullets.isEmpty

  def getNext(): Option[A] = {
    bullets match {
      case Nil => None
      case x :: xs => {
        bullets = xs
        Some(x)
      }
    }
  }

}

object AmmoMagazine {
  def newNormalBulletsMag() = new AmmoMagazine[NormalBullet](List.empty[NormalBullet])

  def newExplosiveBulletsMag() = new AmmoMagazine[ExplosiveBullet](List.empty[ExplosiveBullet])
}

final class Gun(private var ammoMag: AmmoMagazine[Bullet]) {

  def reload(ammoMag: AmmoMagazine[Bullet]): Unit = {
    this.ammoMag = ammoMag
  }

  def hasAmmo: Boolean = ammoMag.hasBullets

  def shoot(): Option[Bullet] = ammoMag.getNext()

}

val gun = new Gun(AmmoMagazine.newNormalBulletsMag)
gun.reload(AmmoMagazine.newExplosiveBulletsMag)

class GarbageCan[-A] {

}

class Item {}

class PlasticItem extends Item {}

def setGarbageCanForPlastic(gc: GarbageCan[PlasticItem]): Unit {

}

// contravariant
// GarbageCan[B] <: Garbage[A] if A :< B
setGarbageCanForPlastic(new GarbageCan[Item])



