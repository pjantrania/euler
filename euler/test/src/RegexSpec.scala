import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import regex._

class RegexSpec extends AnyFlatSpec with should.Matchers {
  "Regex.isMatch" should "match a simple character pattern" in {
    Regex("aab").isMatch("aab") should be(true)
  }
  it should "reject a non-matching simple character pattern" in {
    Regex("aab").isMatch("adab") should be(false)
  }

  it should "reject a string that is shorter than the pattern" in {
    Regex("1234abcd").isMatch("1234a") should be(false) 
  }

  it should "match a wildcard character" in {
    Regex("a.cd").isMatch("azcd") should be(true)
  }

  it should "match a kleene star following a character" in {
    Regex("a*bc").isMatch("aaaaaabc") should be(true)
  }

  it should "match a kleene star pattern with zero matching characters" in {
    Regex("a*bc").isMatch("bc") should be(true)
  }

  it should "match a string with prefix matching pattern" in {
    Regex(".a*bc").isMatch("dbcagdf") should be(true)
  }

  "KleeneStar.process" should "match a single occurrence of a letter" in {
    val t = KleeneStar(Character('a', Success), Success)
    t.process('a') should be(t)
  }

  it should "match multiple occurrences of the same letter" in {
    val t = KleeneStar(Character('a', Success), Success)
    t.process('a').process('a') should be(t)
  }

  it should "match the next expression given non-matching character" in {
    val t = KleeneStar(Character('a', Success), Character('b', Success))
    t.process('b') should be(Success)
    
  }
}
