addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.7")

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")

resolvers += Resolver.bintrayIvyRepo("rallyhealth", "sbt-plugins")

addSbtPlugin("com.rallyhealth.sbt" % "sbt-git-versioning" % "1.2.1")
