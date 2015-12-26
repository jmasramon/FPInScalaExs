/**
  * Created by jordimasramon on 23/12/15.
  */

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import com.softwaremill.react.kafka.KafkaMessages.StringKafkaMessage
import kafka.serializer.{StringDecoder, StringEncoder}
import org.reactivestreams.{Publisher, Subscriber}
import com.softwaremill.react.kafka.{ReactiveKafka, ProducerProperties, ConsumerProperties}


object myMod {
  implicit val actorSystem = ActorSystem("ReactiveKafka")
  implicit val materializer = ActorMaterializer()

  val kafka = new ReactiveKafka()
  val publisher: Publisher[StringKafkaMessage] = kafka.consume(ConsumerProperties(
    brokerList = "192.168.1.59:9192",
    zooKeeperHost = "192.168.1.59:2181",
    topic = "test",
    groupId = "groupName",
    decoder = new StringDecoder()
  ))

  val subscriber: Subscriber[String] = kafka.publish(ProducerProperties(
    brokerList = "192.168.1.59:9192",
    topic = "uppercaseStrings",
    encoder = new StringEncoder()
  ))

  Source(publisher).map(_.message().toUpperCase).to(Sink(subscriber)).run()
}