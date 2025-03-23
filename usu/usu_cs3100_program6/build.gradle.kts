plugins {
    java
}

group = "com.example"  // You can set this to any group name you like
version = "1.0-SNAPSHOT"
java.sourceCompatibility = JavaVersion.VERSION_17  // Use your desired Java version

repositories {
    mavenCentral()
}

dependencies {
    // No dependencies for a simple HelloWorld
}

// Define the source directory explicitly
sourceSets {
    main {
        java {
            srcDir("src")  // Tell Gradle where to find your Java files (directly in src)
        }
    }
}

// Custom run task to execute the Main class
tasks.register("run", JavaExec::class) {
    mainClass.set("Assign6")  // No package, just the class name
    classpath = sourceSets["main"].runtimeClasspath  // Use the classpath for the main source set
}
