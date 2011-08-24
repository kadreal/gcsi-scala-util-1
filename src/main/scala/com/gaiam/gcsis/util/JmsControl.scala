package com.gaiam.gcsis.util

import javax.jms._

/**
 *
 * Ported from Java
 * User: tstevens
 * Date: 4/19/11
 */
class JmsControl(val connectionFactory: ConnectionFactory, val queue: Queue) {

  private final val maxNumMsgs: Int = 800
  private var session: Session = null
  private var producer: MessageProducer = null
  private var con: Connection = null
  private var msgsSent: Int = 0

  def send(create: (Session) => Message) = {
    if (!isConnectionOpen) {
      createConnection
    }
    val m: Message = create(session)
    producer.send(m)
    msgsSent += 1
    if (msgsSent >= maxNumMsgs) {
      closeConnection
    }
  }

  def closeConnection: Unit = {
    if (isConnectionOpen) {
      session.close
      con.close
      con = null
      session = null
      producer = null
    }
  }

  private def isConnectionOpen: Boolean = {
    con != null
  }

  private def createConnection: Unit = {
    con = connectionFactory.createConnection
    session = con.createSession(false, Session.AUTO_ACKNOWLEDGE)
    producer = session.createProducer(queue)
    msgsSent = 0
  }

}