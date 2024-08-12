object Q1 {
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  // Encryption function
  def encrypt(c: Char, shift: Int): Char = {
    if (c.isLetter) {
      val base = if (c.isUpper) 'A' else 'a'
      ((c.toUpper - 'A' + shift) % 26 + 'A').toChar match {
        case result if c.isUpper => result
        case result => result.toLower
      }
    } else c
  }

  // Decryption function
  def decrypt(c: Char, shift: Int): Char = {
    encrypt(c, 26 - shift)
  }

  // General cipher function
  def cipher(text: String, shift: Int, cipherFunc: (Char, Int) => Char): String = {
    text.map(c => cipherFunc(c, shift))
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Hello,I am Anushad and my Index No: is 22000887"
    val shift = 3

    val encrypted = cipher(plaintext, shift, encrypt)
    val decrypted = cipher(encrypted, shift, decrypt)

    println(s"Original text: $plaintext")
    println(s"Encrypted text: $encrypted")
    println(s"Decrypted text: $decrypted")

    // Demonstrate direct use of encrypt and decrypt functions
    val singleCharEncrypted = encrypt('A', shift)
    val singleCharDecrypted = decrypt(singleCharEncrypted, shift)
    println(s"\nSingle character encryption/decryption:")
    println(s"Original: A, Encrypted: $singleCharEncrypted, Decrypted: $singleCharDecrypted")
  }
}