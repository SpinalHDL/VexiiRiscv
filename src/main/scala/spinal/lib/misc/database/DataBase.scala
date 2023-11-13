package spinal.lib.misc.database

import spinal.core._
import spinal.core.fiber._

import scala.collection.mutable

// This is just a HashMap storage
class Database{
  // User API
  def update[T](key: Element[T], value: T) = key.set(this, value)
  def apply[T](key: Element[T]): T = key.get(this)
  def on[T](body: => T) = Database(this).on(body)

  // "private" API
  val storage = mutable.LinkedHashMap[Element[_ <: Any], Any]()
  def storageUpdate[T](key : Element[T], value : T) = storage.update(key, value)
  def storageGet[T](key : Element[T]) : T = storage.apply(key).asInstanceOf[T]
  def storageGetElseUpdate[T](key: Element[T], create: => T): T = storage.getOrElseUpdate(key, create).asInstanceOf[T]
  def storageExists[T](key : Element[T]) = storage.contains(key)
}

// This is the default thread local database instance
object Database extends ScopeProperty[Database] {
  def on[T](body: => T) = this (new Database).on(body)
  def value[T]() = new ElementValue[T]()
  def blocking[T]() = new ElementBlocking[T]()
  def landa[T](body : => T) = new ElementLanda(body)
}

object Element{
  implicit def toValue[T](p : Element[T]) : T = p.get

  class ThingIntPimper(p: Element[Int]) {
    def bits = BitCount(p.get)
  }
  implicit def thingIntPimperFunc(p: Element[Int]) : ThingIntPimper = new ThingIntPimper(p)
}

// Represent a thing which can be in a data base (this is the key)
abstract class Element[T](sp: ScopeProperty[Database] = Database) extends Nameable {
  // user API
  def get(): T = get(sp.get)
  def apply(): T = get(sp.get)
  def set(value: T): Unit = set(sp.get, value)
//  implicit def toValue() : T = get()

  // private API
  def get(db: Database) : T
  def set(db: Database, value: T) : Unit
}

// Simple implementation
class ElementValue[T](sp : ScopeProperty[Database] = Database) extends Element[T](sp) {
  def get(db: Database): T = db.storageGet(this)
  def set(db: Database, value: T) = db.storageUpdate(this, value)
}

// Layered with a handle to allow blocking "get"
class ElementBlocking[T](sp : ScopeProperty[Database] = Database) extends Element[T](sp) with Area{
  val thing = new ElementValue[Handle[T]]()
  def getHandle(db : Database) : Handle[T] = db.storageGetElseUpdate(thing, new Handle[T].setCompositeName(this))
  def get(db: Database) : T = getHandle(db).get
  def set(db: Database, value : T) = {
    assert(!getHandle(db).isLoaded)
    getHandle(db).load(value)
  }
}

// The body provide the processing to generate the value
class ElementLanda[T](body : => T, sp : ScopeProperty[Database] = Database) extends ElementValue[T](sp){
  override def get(db: Database) : T = {
    if(!db.storageExists(this)){
      db.storageUpdate(this, body)
    }
    super.get(db)
  }

  override def set(db: Database, value: T) = ???
}
