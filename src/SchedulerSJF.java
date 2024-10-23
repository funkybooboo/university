class SchedulerSJF extends Scheduler {
    Logger logger;

    public SchedulerSJF(Logger logger) {
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
