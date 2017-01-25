object Test {
    List(
        List("en", "as", "my"),
        List("en", "my", "as"),
        List("man", "yes"),
        List("men", "say"),
        List("as", "en", "my"),
        List("as", "my", "en"),
        List("sane", "my"),
        List("Sean", "my"),
        List("my", "en", "as"),
        List("my", "as", "en"),
        List("my", "sane"),
        List("my", "Sean"),
        List("say", "men"),
        List("yes", "man")
    ).size

    trait Coin {}
    case class Gold(x: Int, y: Int) extends Coin {
        val zz;
        def this(x: Int, y: Int, z: Int) {
            this(x, y)
            zz = z;
        }
    }
    case class Silver() extends Coin {}

    class Banana(x: Int, y: Int) {
        def this(x: Int, y: Int, z: Int) {
        }
    }
}
