package cilly

/**
 * Quoting from  the CIL spec, Partition II, Sec. 7.1.1:
 *
 * Custom modifiers, defined using `modreq` (required modifier) and `modopt` (optional modifier), are
 * similar to custom attributes (Sec. 21) except that modifiers are part of a signature rather than being attached to a
 * declaration. Each modifer associates a type reference with an item in the signature.
 *
 */
object CustomModifier {
  def helperCustomMods(isReqd: Boolean, cmods: Array[CustomModifier]): Array[Type] = {
    if (cmods == null) return null
    var count: Int = 0

    {
      var idx: Int = 0
      while (idx < cmods.length) {
        {
          if (cmods(idx).isReqd == isReqd) ({
            count += 1; count - 1
          })
        }
        ({
          idx += 1; idx - 1
        })
      }
    }
    val res: Array[Type] = new Array[Type](count)
    var residx: Int = 0

    {
      var idx: Int = 0
      while (idx < cmods.length) {
        {
          res(residx) = cmods(idx).marker
          residx += 1
        }
        ({
          idx += 1; idx - 1
        })
      }
    }
    res
  }

  def VolatileMarker: Type = Type.getType("System.Runtime.CompilerServices.IsVolatile")
}

class CustomModifier(val isReqd: Boolean, val marker: Type) {
  override def toString: String = (if (isReqd) "modreq( " else "modopt( ") + marker.toString + " )"
}