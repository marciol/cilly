/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly

import java.security.MessageDigest

/**
 * Fully describes an assembly's unique identity. Right now it's only the name
 *
 * The simple, unencrypted name of the assembly.
 * Gets or sets the major, minor, revision, and build numbers of the assembly.
 * 
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final class AssemblyName(val name: String, val version: Version) {

  /** Gets a strong name consisting of a public key, a given name, and version parts. */
  def publicKeyToken: Array[Byte] = if (_publicKeyToken == null) null else _publicKeyToken.clone.asInstanceOf[Array[Byte]]

  /** Sets a strong name consisting of a public key, a given name, and version parts. */
  def setPublicKeyToken(key: Array[Byte]) = {
    this._publicKeyToken = if (key.length == 0) null else key.clone.asInstanceOf[Array[Byte]]
  }

  /** Returns the public key identifying the originator of the assembly. */
  def publicKey = if (_publicKey == null) null else _publicKey.clone.asInstanceOf[Array[Byte]]

  /** Sets the public key identifying the originator of the assembly. */
  def setPublicKey(key: Array[Byte]) = {
    if (key.length > 0) {
      this._publicKey = key.clone()
      val hash: Array[Byte] = AssemblyName.SHA.digest(key)
      val keyToken: Array[Byte] = new Array[Byte](8)

      var i = 0
      while (i < keyToken.length) {
        keyToken(i) = hash(hash.length - 1 - i)
        i += 1
      }

      this._publicKeyToken = keyToken;
      // System.out.println("Pubic key and key token of assembly " + this
      // + ":");
      // System.out.println("\tPublic key = " + Table.bytes2hex(key));
      // System.out.println("\tKey token  = " +
      // Table.bytes2hex(keyToken));
    }
  }

  override def toString = s"$name, Version=$version"

  private var _publicKeyToken: Array[Byte] = null

  private var _publicKey: Array[Byte] = null
}

final object AssemblyName {
  final val SHA: MessageDigest = try {
    MessageDigest.getInstance("SHA")
  } catch {
    case _: java.security.NoSuchAlgorithmException =>
      null
  }
}
