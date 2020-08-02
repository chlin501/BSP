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

import zio._
import zio.arrow.ZArrow

trait Step[I, O, T[_, _], T1[_]] {

  trait StepResult[T1[_]] {
    def get: T1[_]
  }

  def |[O1](next: Step[O, O1, T, T1]): Step[I, O1, T, T1]

  def underlying: T[I, O]

  def run(in: I): StepResult[T1]

}
object Step {

  type Z[I, O] = ZArrow[Any, I, O]
  type ZO[O] = IO[Any, O]

  case class ZIOStep[I, O](arrow: Z[I, O]) extends Step[I, O, Z, ZO] {

    case class ZIOResult[O](io: ZO[O]) extends StepResult[ZO] {
      def get: ZO[O] = io
    }

    override def |[O1](next: Step[O, O1, Z, ZO]): Step[I, O1, Z, ZO] =
      ZIOStep[I, O1](arrow >>> next.underlying)

    override def underlying: Z[I, O] = arrow

    override def run(in: I): StepResult[ZO] = ZIOResult[O](arrow.run(in))

  }

  def zioStep[I, O](f: I => O): ZIOStep[I, O] = ZIOStep[I, O](ZArrow(f))

}
