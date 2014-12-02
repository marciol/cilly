package cilly.util;

import cilly.Type;
import cilly.CustomModifier;

/**
 * A PECustomMod holds the info parsed from metadata per the CustomMod production in Sec. 23.2.7, Partition II.
 *
 * Terminology:
 *   the CustomModifier(s) are markers,
 *   and the msil.Type is a type marked by those markers.
 */
final case class PECustomMod(marked: Type, cmods: Array[CustomModifier])
