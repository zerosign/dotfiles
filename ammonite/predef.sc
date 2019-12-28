final object packages {
  lazy val versions = Map(
    "http4s" -> "0.21.0-M6",
    "cats-effect" -> "2.0.0",
    "scodec-bits" -> "1.1.12",
    "scodec-core" -> "1.11.4",
    "fs2" -> "2.1.0",
    "circe" -> "0.12.3",
    "circe-yaml" -> "0.12.0",
    "log4s" -> "1.8.2",
    "logback" -> "1.3.0-alpha5",
    "logstash" -> "6.3",
    "atto" -> "0.7.2",
    "scalacache" -> "0.28.0",
    "sqlite" -> "3.30.1",
    "enumeratum" -> "1.5.14",
    "fastparse" -> "2.2.2",
    "quill" -> "3.5.0",
    "scalatags" -> "0.8.3",
    "scalacss" -> "0.6.0-RC1",
    "refined" -> "0.9.10",
    "skunk" -> "0.0.7",
    "doobie" -> "0.8.8",
    "shapeless" -> "2.3.3",
    "graal" -> "19.3.0.2",
    "hashed-timer" -> "1.0.0-RC1",
    "rocksdb" -> "6.3.6",
    "prometheus" -> "0.7.0",
  )

  @inline final def loadIvy(n: String, p: String, v: String) =
    interp.load.ivy(n %% p % v)

  @inline final def loadJava(n: String, p: String, v: String) =
    interp.load.ivy(n % p % v)

  @inline final def typelevel(p: String, version: String) =
    loadIvy("org.typelevel", p, version)

  @inline final def fs2(p: String, version: String) =
    loadIvy("co.fs2", p, version)

  @inline final def circe(p: String, version: String) =
    loadIvy("io.circe", p, version)

  @inline final def shapeless(version: String) =
    loadIvy("com.chuusai", "shapeless", version)

  @inline final def tpolecat(p: String, version: String) =
    loadIvy("org.tpolecat", p, version)

  @inline final def scodec(p: String, version: String) =
    loadIvy("org.scodec", p, version)

  @inline final def scalacache(p: String, version: String) =
    loadIvy("com.github.cb372", p, version)

  @inline final def quill(p: String, version: String) =
    loadIvy("io.getquill", p, version)

  @inline final def refined(p: String, version: String) =
    loadIvy("eu.timepit", p, version)

  @inline final def lihaoyi(p: String, version: String) =
    loadIvy("com.lihaoyi", p, version)

  @inline final def scalacss(p: String, version: String) =
    loadIvy("com.github.japgolly.scalacss", p, version)

  @inline final def http4s(p: String, version: String) =
    loadIvy("org.http4s", p, version)

}

packages.typelevel("cats-effect", packages.versions("cats-effect"))
packages.fs2("fs2-io", packages.versions("fs2"))
packages.shapeless(packages.versions("shapeless"))

Seq("circe-core", "circe-parser", "circe-generic").foreach(
  packages.circe(_, packages.versions("circe"))
)

packages.circe("circe-yaml", packages.versions("circe-yaml"))

Seq("atto-core", "atto-refined").foreach(
  packages.tpolecat(_, packages.versions("atto"))
)

Seq("doobie-core", "doobie-postgres", "doobie-hikari", "doobie-h2").foreach(
  packages.tpolecat(_, packages.versions("doobie"))
)

Seq("scodec-core", "scodec-bits").foreach { p =>
  packages.scodec(p, packages.versions(p))
}

Seq("scalacache-core", "scalacache-cats-effect", "scalacache-memcached", "scalacache-redis").foreach(
  packages.scalacache(_, packages.versions("scalacache"))
)

Seq("quill-core", "quill-jdbc", "quill-sql").foreach(
  packages.quill(_, packages.versions("quill"))
)

Seq("refined", "refined-shapeless", "refined-scodec").foreach(
  packages.refined(_, packages.versions("refined"))
)

packages.lihaoyi("fastparse", packages.versions("fastparse"))
packages.lihaoyi("scalatags", packages.versions("scalatags"))

Seq("skunk-core", "skunk-circe", "skunk-macros").foreach(
  packages.tpolecat(_, packages.versions("skunk"))
)

Seq("core", "ext-scalatags").foreach(
  packages.scalacss(_, packages.versions("scalacss"))
)

Seq(
  "http4s-core",
  "http4s-dsl",
  "http4s-client",
  "http4s-server",
  "http4s-circe",
  "http4s-blaze-client",
  "http4s-blaze-server"
).foreach(
  packages.http4s(_, packages.versions("http4s"))
)

packages.loadIvy("com.beachape", "enumeratum", packages.versions("enumeratum"))
packages.loadIvy("com.beachape", "enumeratum", packages.versions("enumeratum"))

// rewrite this packages
// packages.loadIvy("com.github.ifesdjeen", "hashed-wheel-timer-core", packages.versions("hashed-timer"))

packages.loadJava("org.graalvm.sdk", "graal-sdk", packages.versions("graal"))
packages.loadJava("io.prometheus", "simpleclient", packages.versions("prometheus"))
packages.loadJava("org.xerial", "sqlite-jdbc", packages.versions("sqlite"))
packages.loadJava("org.rocksdb", "rocksdbjni", packages.versions("rocksdb"))

@
