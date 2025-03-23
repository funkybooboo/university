# Chapter 16

**Purpose:**

- **Overview of security threats and attacks**
- **Fundamentals of encryption, authentication, and hashing**
- **Applications of cryptography in security**
- **Description of various countermeasures**

This chapter serves as a broad introduction to security, covering foundational concepts and an overview of threats. It introduces basic terms and principles, though it may feel dense due to the variety of topics covered. We'll define the key concepts as they emerge to maintain clarity.

---

### The Security Problem (16.1)

**1. Understanding Security Threats:**

Any system can be the target of an attack. The motivations behind these attacks can vary:

- **Personal Data**
- **Monetary Gains**
- **CPU Cycles** (e.g., using system resources for mining or bots)
- **Blackmail**
- **Thrill/Boredom/Reputation** (e.g., hacking for fun or fame)

**2. Key Definitions:**

- **Secure:**  
  A system is considered secure if all resources are used and accessed as intended, under all circumstances. This is often idealistic because real-world systems are complex and vulnerable.

- **Violations:**  
  Any misuse of a system or its resources. Violations can be accidental or malicious and are determined by the system's policies.

- **Attacker:**  
  Any entity (person, system, or group) attempting to breach security. Commonly referred to as an intruder or hacker.

- **Threat:**  
  The potential for a security violation. A threat does not always mean a violation is possible; it’s the potential for harm.

- **Attack:**  
  An actual attempt to violate system security. It involves using vulnerabilities to achieve malicious goals.

- **Vulnerability:**  
  A weakness in a system that could potentially be exploited to compromise security.

- **Exploit:**  
  The act of taking advantage of a vulnerability. Unlike an attack, which could target a system without weaknesses, an exploit specifically takes advantage of a known flaw.

---

**3. Types of Attacks:**

**Security Triad (CIA):**  
The core goals of system security are to protect the **Confidentiality**, **Integrity**, and **Availability** of data:

- **Breach of Confidentiality:**  
  Unauthorized access to data, meaning someone reads data without permission.

- **Breach of Integrity:**  
  Unauthorized modification of data. This addresses the question, "Is the data correct?"

- **Breach of Availability:**  
  Disrupting authorized access to data or services when they are needed.

These three categories (Confidentiality, Integrity, and Availability) are often referred to as the **CIA Triad**. The book mentions additional attack types, but we’ll focus on these core ones.

**Examples of Attacks:**

- **Masquerading (Impersonation):**  
  When an attacker pretends to be an authorized user. This affects **authentication**, and breaches **confidentiality** and **integrity**.

- **Replay Attack:**  
  The attacker sends a duplicate of a message, typically to trick systems into processing it again (e.g., financial transactions). This primarily affects **integrity**.

- **Man-in-the-Middle Attack (MitM):**  
  The attacker intercepts communication between two parties. This can lead to:
    - **Eavesdropping** (confidentiality breach)
    - **Message modification** (integrity breach)
    - **Message injection** (integrity and availability breach)
    - **Message dropping** (availability breach)

**Other Notable Attack Types:**

- **Session Hijacking:**  
  Taking over an authenticated session between two parties (e.g., stealing a user’s login session).

- **Privilege Escalation:**  
  Gaining higher privileges (e.g., administrator access) to perform unauthorized actions.

- **Denial of Service (DoS):**  
  An attack designed to prevent legitimate users from accessing a service.
    - **Distributed Denial of Service (DDoS):**  
      A form of DoS using multiple compromised systems to launch a coordinated attack, making it harder to stop.

---

### Security Measures

**1. Levels of Security Protection:**

Security measures can be implemented at different levels:

- **Human:**  
  Training and awareness to prevent errors and improve decision-making. Social engineering defenses like awareness of phishing attacks fall here.

- **Physical:**  
  Protecting hardware and physical infrastructure. This includes access control and surveillance.

- **Network:**  
  Security protocols to secure communications (e.g., VPNs, firewalls, encryption).

- **Operating System (OS):**  
  Security features at the system level, such as access control, user authentication, and audit logs.

- **Applications:**  
  Secure coding practices, patch management, and application-level defenses against attacks like SQL injection or cross-site scripting (XSS).

---

### Threat Priorities & Human Issues

**Human Factors in Security:**

- **Social Engineering:**  
  Attackers manipulate people into divulging confidential information or performing actions that compromise security. Common forms include phishing and pretexting.

- **Phishing:**  
  Deceptive attempts to acquire sensitive information by impersonating a trustworthy entity, often through email or fake websites.
