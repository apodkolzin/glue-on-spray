## Glue repository as a spray service

Follow these steps to get started:

1. Git-clone this repository.

        $ git clone https://github.com/apodkolzin/glue-on-spray.git

2. Change directory into your clone:

        $ cd glue-on-spray

3. Launch SBT:

        $ sbt

4. Compile everything and run all tests:

        > test

5. Start the application:

        > re-start

6. Initialize [http://localhost:8080/init](http://localhost:8080/init)

7. Get a root [http://localhost:8080/root](http://localhost:8080/root)

8. Stop the application:

        > re-stop

9. Start hacking on `GlueService.scala`
