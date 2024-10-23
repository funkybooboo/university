class SchedulerRR extends Scheduler {
    Logger logger;
    int q;

    public SchedulerRR(Logger logger, int q) {
        this.logger = logger;
        this.q = q;
    }

    @Override
    void notifyNewProcess(Process process) {

    }

    @Override
    Process update(Process process, int cpu) {
        return null;
    }
}
