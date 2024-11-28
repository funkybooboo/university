## Acronyms

### DMA: Direct Memory Access

A feature that allows hardware devices to access the main system memory independently of the CPU, enhancing system
performance during data transfers.

### SMP: Symmetric Multiprocessing

An architecture where two or more identical processors share the same main memory, allowing them to work on tasks
simultaneously for improved performance and reliability.

### MBR: Master Boot Record

A boot sector located at the beginning of a storage device that contains the partition table and boot loader code,
crucial for starting the operating system.

### UEFI: Unified Extensible Firmware Interface

A modern firmware interface that replaces the traditional BIOS. UEFI provides a more flexible pre-boot environment,
allowing for faster boot times, larger disk support (over 2 TB), and enhanced security features like Secure Boot. It
includes a graphical interface, supports booting from various file systems, and allows for greater customization and
management of system settings.

### GUID: Globally Unique Identifier

### GPT: The GUID Partition Table

A modern partitioning scheme that supports larger disks (over 2 TB) and allows up to 128 partitions without needing an
extended partition. It offers redundancy by storing multiple copies of partition data and includes CRC protection to
detect corruption. GPT is part of the UEFI standard and is widely supported by modern operating systems, making it the
preferred choice for new drives.

### ISR: Interrupt Service Routine

An Interrupt Service Routine (ISR) is a special block of code in embedded systems or operating systems that executes in
response to an interrupt signal. When a hardware device needs attention, it sends an interrupt to the CPU, which pauses
its current tasks to execute the ISR associated with that interrupt. This allows the system to handle events like input
from peripherals or timer expirations efficiently. After the ISR completes its task, control returns to the interrupted
process, ensuring that critical operations are managed promptly without significant delay.

### CIA

- **Confidentiality**: Ensures that information is only accessible to authorized individuals through measures like
  encryption, access controls, and authentication.
- **Integrity**: Ensures data is accurate, authentic, and reliable, protecting against unauthorized modifications.
  Techniques such as checksums, hashing, and digital signatures are commonly used.
- **Availability**: Ensures that information and resources are accessible to authorized users when needed, utilizing
  measures like redundancy, fail over systems, and regular maintenance to prevent downtime.

### PCB: Process Control Block

A vital data structure in an operating system that stores essential information about a process, such as its unique
Process ID (PID), current state, and program counter. It facilitates context switching by saving and restoring the CPU
registers and scheduling information, enabling efficient multitasking. Additionally, the PCB tracks memory management
and I/O status, allowing the operating system to manage resources and process communication effectively.

