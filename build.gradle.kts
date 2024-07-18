plugins {
    kotlin("jvm")
}

group = "org.natestott"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("io.ktor:ktor-server-core:2.3.12")
    implementation("io.ktor:ktor-server-netty:2.3.12")
    implementation("io.ktor:ktor-server-websockets:2.3.12")
    implementation("org.slf4j:slf4j-api:1.7.36")
    implementation("ch.qos.logback:logback-classic:1.2.6")
    testImplementation(kotlin("test"))
}

tasks {
    processResources {
        duplicatesStrategy = DuplicatesStrategy.INCLUDE // Handle duplicates by including all files
    }
}

sourceSets {
    main {
        resources.srcDirs("src/main/resources")
    }
}
