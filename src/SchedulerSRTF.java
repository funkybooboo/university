class SchedulerSRTF extends Scheduler {
    Logger logger;

    public SchedulerSRTF(Logger logger) {
        this.logger = logger;
    }

    @Override
    void notifyNewProcess(Process process) {

    }

    @Override
    Process update(Process process, int cpu) {
        return null;
    }
}
