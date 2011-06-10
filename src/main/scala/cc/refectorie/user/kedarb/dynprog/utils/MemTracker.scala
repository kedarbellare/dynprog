package cc.refectorie.user.kedarb.dynprog.utils

class MemTracker {
  private var initMem: Long = 0
  private var initTime: Long = 0

  private def gc = System.gc

  private def getMemoryUsage = (Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory) / (1024 * 1024)

  def start = {
    gc
    gc
    gc
    initMem = getMemoryUsage
    initTime = System.currentTimeMillis
  }

  def stop(name: String, puts: String => Any = println(_)) = {
    gc
    gc
    gc
    puts(name + " used: " + (getMemoryUsage - initMem) +
      " MB (time: " + (System.currentTimeMillis - initTime) / 1000 + " s)")
  }
}