FROM openjdk:8-jre-alpine
MAINTAINER Louis Pilfold <louis@lpil.uk>

ADD ./target/hello-world.jar .

RUN adduser -S app
USER app

CMD ["java", "-jar", "./hello-world.jar"]
