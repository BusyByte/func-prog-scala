package net.nomadicalien

import scala.concurrent.ExecutionContext


package object promise {
  type ExecutionException =    java.util.concurrent.ExecutionException
  type CancellationException = java.util.concurrent.CancellationException
  type TimeoutException =      java.util.concurrent.TimeoutException

  /** Used to designate a piece of code which potentially blocks, allowing the current [[BlockContext]] to adjust
    *  the runtime's behavior.
    *  Properly marking blocking code may improve performance or avoid deadlocks.
    *
    *  Blocking on an [[Awaitable]] should be done using [[Await.result]] instead of `blocking`.
    *
    *  @param body         A piece of code which contains potentially blocking or long running calls.
    *  @throws CancellationException if the computation was cancelled
    *  @throws InterruptedException in the case that a wait within the blocking `body` was interrupted
    */
  @throws(classOf[Exception])
  def blocking[T](body: =>T): T = BlockContext.current.blockOn(body)(AwaitPermission)
}

package promise {

  import scala.annotation.implicitNotFound
  import scala.concurrent.duration.Duration

  /**
    * This marker trait is used by [[Await]] to ensure that [[Awaitable.ready]] and [[Awaitable.result]]
    * are not directly called by user code. An implicit instance of this trait is only available when
    * user code is currently calling the methods on [[Await]].
    */
  @implicitNotFound("Don't call `Awaitable` methods directly, use the `Await` object.")
  sealed trait CanAwait

  /**
    * Internal usage only, implementation detail.
    */
  private[promise] object AwaitPermission extends CanAwait

  /**
    * `Await` is what is used to ensure proper handling of blocking for `Awaitable` instances.
    *
    * While occasionally useful, e.g. for testing, it is recommended that you avoid Await
    * when possible in favor of callbacks and combinators like onComplete and use in
    * for comprehensions. Await will block the thread on which it runs, and could cause
    * performance and deadlock issues.
    */
  object Await {
    /**
      * Await the "completed" state of an `Awaitable`.
      *
      * Although this method is blocking, the internal use of [[scala.concurrent.blocking blocking]] ensures that
      * the underlying [[ExecutionContext]] is prepared to properly manage the blocking.
      *
      * @param  awaitable
      *         the `Awaitable` to be awaited
      * @param  atMost
      *         maximum wait time, which may be negative (no waiting is done),
      *         [[scala.concurrent.duration.Duration.Inf Duration.Inf]] for unbounded waiting, or a finite positive
      *         duration
      * @return the `awaitable`
      * @throws InterruptedException     if the current thread is interrupted while waiting
      * @throws TimeoutException         if after waiting for the specified time this `Awaitable` is still not ready
      * @throws IllegalArgumentException if `atMost` is [[scala.concurrent.duration.Duration.Undefined Duration.Undefined]]
      */
    @throws(classOf[TimeoutException])
    @throws(classOf[InterruptedException])
    def ready[T](awaitable: Awaitable[T], atMost: Duration): awaitable.type =
    blocking(awaitable.ready(atMost)(AwaitPermission))

    /**
      * Await and return the result (of type `T`) of an `Awaitable`.
      *
      * Although this method is blocking, the internal use of [[scala.concurrent.blocking blocking]] ensures that
      * the underlying [[ExecutionContext]] to properly detect blocking and ensure that there are no deadlocks.
      *
      * @param  awaitable
      *         the `Awaitable` to be awaited
      * @param  atMost
      *         maximum wait time, which may be negative (no waiting is done),
      *         [[scala.concurrent.duration.Duration.Inf Duration.Inf]] for unbounded waiting, or a finite positive
      *         duration
      * @return the result value if `awaitable` is completed within the specific maximum wait time
      * @throws InterruptedException     if the current thread is interrupted while waiting
      * @throws TimeoutException         if after waiting for the specified time `awaitable` is still not ready
      * @throws IllegalArgumentException if `atMost` is [[scala.concurrent.duration.Duration.Undefined Duration.Undefined]]
      */
    @throws(classOf[Exception])
    def result[T](awaitable: Awaitable[T], atMost: Duration): T =
    blocking(awaitable.result(atMost)(AwaitPermission))
  }
}
