package cilly

trait HasCustomModifiers {
  def getOptionalCustomModifiers: Array[Type]
  def getRequiredCustomModifiers: Array[Type]
}
