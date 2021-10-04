package exercises

object Concurrency extends App:
  def createThread(maxThreads: Int, i: Int = 1): Thread =
    new Thread(() => {
      if i < maxThreads then {
        val newThread = createThread(maxThreads, i + 1)
        newThread.start()
        newThread.join()
      }
      println(s"Hello from thread #$i")
    })

  createThread(50).start()

