import mill._
import scalalib._

trait CommonModule extends ScalaModule {
  override def scalaVersion = "2.13.0-M5"

  override def ivyDeps = Agg(
    ivy"com.lihaoyi::utest:0.6.6+14-2aed375c",
    ivy"com.lihaoyi::fastparse:2.1.0"
  )

  object test extends Tests with TestModule {
    def testFrameworks = Seq("utest.runner.Framework")
  }

  override def scalacOptions = Seq(
    "-target:jvm-1.8",                   // JVM 1.8
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-unchecked",                        // Enable additional warnings.
    "-deprecation",                      // Enable warnings on deprecated usage.
    "-feature",                          // Enable warnings for features requiring explicit import.
    "-language:implicitConversions",     // Enable implicit conversions.
    "-Xfatal-warnings",                  // Change warnings to errors.
    "-Yno-generic-signatures",           // Suppress generation of generic signatures in bytecode.
    "-Xfuture",                          // Enable "future language features".
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-opt:l:method,inline",              // Enable method optimizations, inlining
    "-opt-warnings:none",                // Disable optimization warnings
    "-explaintypes",                     // Explain type errors in more detail.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:privates"             // Warn if a private member is unused.
  )
}

object utils extends CommonModule {

}

object ichor extends CommonModule {

  override def moduleDeps = Seq(utils)

}

object eos extends CommonModule {
  override def moduleDeps = Seq(ichor)
}