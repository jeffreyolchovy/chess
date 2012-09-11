package com.olchovy.chess.domain.event

/* Error condition for any invalid move attempt */
case class MoveFailure(attempt: MoveAttempt, private val message: String) extends Throwable(message)
