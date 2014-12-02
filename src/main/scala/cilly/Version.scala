/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly

/**
 * Represents the version number for a common language runtime assembly
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final case class Version(major: Int, minor: Int, build: Int, revision: Int) {
  override def toString = s"$major.$minor.$build.$revision"
}
