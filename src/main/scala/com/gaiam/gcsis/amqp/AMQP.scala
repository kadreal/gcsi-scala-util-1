package com.gaiam.gcsis.amqp

import com.gaiam.gcsis.util.Logging.logger
import com.rabbitmq.client.QueueingConsumer.Delivery
import com.rabbitmq.client._
import scala.xml._
import com.gaiam.gcsis.util.FS.implicits._

case class AmqpProcessingError(message: String, error: Option[Throwable])

object AMQP {
    val log = logger[AMQP]
    def connectionFactory(user: String, password: String, vhost: String, host: String, port: Int) = {
        new ConnectionFactory ->- {factory =>
          factory.setUsername(user)
          factory.setPassword(password)
          factory.setVirtualHost(vhost)
          factory.setRequestedHeartbeat(0)
          factory.setPort(port)
          factory.setHost(host)

          if (port == 5671) {
            factory.useSslProtocol()
          }
        }
    }
}

trait AMQP {
    import AMQP._

    def factory: ConnectionFactory


    def publishXml(exchange: String, key: String)(f: => Elem) : Either[AmqpProcessingError, String] = {
      try {
        withChannel(channel => {

            val message = f.toString()
            log.info("Publishing xml" + message)
            channel.basicPublish(exchange, key, MessageProperties.PERSISTENT_TEXT_PLAIN, message.getBytes)
            Right(message) //no processing errors
        })
      } catch {
        case e: Exception => Left(AmqpProcessingError("Could not publish message to exchange:" + exchange + " key: " + key, Some(e)))
      }
    }

    private[this] def nextDelivery(consumer: QueueingConsumer, timeout: Option[Long]) : Option[Delivery]  = {
      val next = try {
        Option(timeout.map(consumer.nextDelivery(_)).getOrElse(consumer.nextDelivery))
      } catch {
        case e1: InterruptedException => None
        case e2: ShutdownSignalException => None
        case x: Exception => None
      }
      next
    }

    private[this] def toXml(body: Array[Byte]) : Elem = {
      xml.XML.loadString(new String(body, "UTF-8"))
    }

    def consumeXml(exchange: String, key: String, timeout: Option[Long] = None)(handler : Elem => Boolean) = {
      withChannel( channel => {
        val consumer = new QueueingConsumer(channel)
        channel.basicConsume(exchange, false, consumer)

        Right(handleDeliveries(channel, consumer, timeout, nextDelivery(consumer, timeout))(handler))

      })
    }

    final def handleDeliveries(channel: Channel, consumer: QueueingConsumer, timeout: Option[Long], del: Option[QueueingConsumer.Delivery])(handler: Elem => Boolean) {
      if (del.isDefined) {
        del.foreach {
          d => if (handler.apply(toXml(d.getBody))) {
            channel.basicAck(d.getEnvelope.getDeliveryTag, false)
          }
        }

        handleDeliveries(channel, consumer, timeout, nextDelivery(consumer, timeout))(handler)
      }
    }

    def withChannel[T](f: (Channel) => Either[AmqpProcessingError, T]) : Either[AmqpProcessingError, T] = {
        try {
            val conn = factory.newConnection
            val channel = conn.createChannel

            try {
                f(channel)
            } catch {
                case e: Exception => {
                  val message = "Error sending message " + f.toString + " to " + factory.getHost
                  log.error(message, e)
                  Left(AmqpProcessingError(message, Some(e)))
                }
            } finally {
                channel.close(-1, "Closing during normal channel lifecycle.")
                conn.close(-1, "Closing during normal connection lifecycle.")
            }
        } catch {
            case e: Exception => {
              val message = "Error connecting " + f.toString + " to " + factory.getHost
              log.error(message, e)
              Left(AmqpProcessingError(message, Some(e)))
            }
        }
    }
}
