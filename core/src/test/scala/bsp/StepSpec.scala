/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package bsp

import org.scalatest._
import flatspec._
import matchers._
import scala.collection.mutable.ArraySeq
import scala.util.Either
import zio._

class StepSpec extends AnyFlatSpec with should.Matchers {

  "Step" should "be composable" in {

    import Step._

    type E[A] = Either[Throwable, A]

    val expected = Right(ArraySeq('1', '2', '3', '.', '0', '_', 'd', 'o', 'u', 'b', 'l', 'e'))

    val step1 = zioStep[Int, E[Double]]( (int: Int) =>
      if(int > 0) Right(int.toDouble) else Left(new RuntimeException(s"$int smaller than 0!"))
    )

    val step2 = zioStep[E[Double], E[String]]( (doubleE: E[Double]) => doubleE.map { double =>
      if (double % 10.0 > 1) double.toString + "_double" else "3.0"
    })

    val step3 = zioStep[E[String], E[Seq[Char]]]( (strE: E[String]) =>
      strE.map(_.toCharArray.toSeq)
    )

    val io = ((step1 | step2 | step3) run 123).get
    val runtime = Runtime.default

    assertResult(expected)(runtime.unsafeRun(io))

  }

}
