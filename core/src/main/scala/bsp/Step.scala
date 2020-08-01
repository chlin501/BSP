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

import zio.arrow.ZArrow

trait Step[I, O, T[_, _]] {

  def |[O1](next: Step[O, O1, T]): Step[I, O1, T]

  def underlying: T[I, O]

}
object Step {

  type Z[I, O] = ZArrow[Any, I, O]

  case class ZIOStep[I, O](arrow: Z[I, O]) extends Step[I, O, Z] {

    override def |[O1](next: Step[O, O1, Z]): Step[I, O1, Z] =
      ZIOStep[I, O1](arrow >>> next.underlying)

    override def underlying: Z[I, O] = arrow

  }

}
/*
class Step[I, O](k: Kleisli[Result, I, O]) {

  /**
 * Form a new computation by taking in input I, and produce output O1.
 */
  def |[O1](next: Step[O, O1]) = new Step[I, O1](
    k andThen sync() andThen next.kleisli
  )

  protected[bsp] def kleisli = k

  def run(in: I) = k.run(in)

  protected[bsp] def sync() = Kleisli[Result, O, O]( (o: O) => {
    /* TODO: barrier sync() operation */
    Success(o)
  })

}
 */
