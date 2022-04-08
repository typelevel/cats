/*
 * Copyright (c) 2022 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats.platform.Platform
import org.scalacheck.{Test => ScalaCheckTest}

trait TestSettings {

  lazy val checkConfiguration: ScalaCheckTest.Parameters =
    ScalaCheckTest.Parameters.default
      .withMinSuccessfulTests(if (Platform.isJvm) 50 else 5)
      .withMaxDiscardRatio(if (Platform.isJvm) 5.0f else 50.0f)
      .withMaxSize(if (Platform.isJvm) 10 else 5)
      .withMinSize(0)
      .withWorkers(if (Platform.isJvm) 2 else 1)

  lazy val slowCheckConfiguration: ScalaCheckTest.Parameters =
    if (Platform.isJvm) checkConfiguration
    else
      ScalaCheckTest.Parameters.default
        .withMinSuccessfulTests(1)
        .withMaxSize(ScalaCheckTest.Parameters.default.minSize + 1)
}

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Cats tests.
 */
trait CatsSuite extends munit.DisciplineSuite with TestSettings {

  implicit override def scalaCheckTestParameters: ScalaCheckTest.Parameters =
    checkConfiguration
}

trait SlowCatsSuite extends CatsSuite {
  implicit override def scalaCheckTestParameters: ScalaCheckTest.Parameters =
    slowCheckConfiguration
}
