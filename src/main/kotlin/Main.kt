import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.runtime.remember
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Window
import androidx.compose.ui.window.application
import kotlinx.coroutines.*
import listener.FileReader
import listener.Queue
import logger.CompositeLogger
import logger.ConsoleLogger
import logger.FileLogger
import logger.Logger.Level
import observer.TrackerViewHelper
import subject.Shipment
import subject.update.*
import java.util.*

val logger = CompositeLogger()

fun main() = runBlocking {

    val fileLogger = FileLogger("log/logs.log")
    val consoleLogger = ConsoleLogger()
    logger.registerLogger(consoleLogger)
    logger.registerLogger(fileLogger)

    // Configuration
    val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
        Pair("created", ::Created),
        Pair("shipped", ::Shipped),
        Pair("location", ::Location),
        Pair("delivered", ::Delivered),
        Pair("delayed", ::Delayed),
        Pair("lost", ::Lost),
        Pair("canceled", ::Canceled),
        Pair("noteadded", ::NoteAdded),
    )
    val listenerFilePath = "data/test.txt"
    val delimiter = ","
    val waitTimeMills = 1000L

    val queue: Queue<String> = Queue()
    val trackingSimulator = TrackingSimulator(typeToUpdateConstructor, delimiter, waitTimeMills, queue)
    val trackerViewHelper = TrackerViewHelper()

    launch {
        val fileReader = FileReader(queue, listenerFilePath)
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Start listening")
        fileReader.listen()
    }
    launch {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Start simulation")
        trackingSimulator.run()
    }
    application {
        Window(onCloseRequest = ::exitApplication) {
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Start ui")
            App(trackerViewHelper, trackingSimulator)
        }
    }
}

@Composable
fun App(
    initialTrackerViewHelper: TrackerViewHelper,
    trackingSimulator: TrackingSimulator
) {
    var searchedShipmentId by remember { mutableStateOf("") }
    var snackbarVisible by remember { mutableStateOf(false) }
    val trackerViewHelper by remember { mutableStateOf(initialTrackerViewHelper) }

    MaterialTheme {
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(16.dp)
        ) {
            Row(
                modifier = Modifier.fillMaxWidth()
            ) {
                TextField(
                    value = searchedShipmentId,
                    onValueChange = { text ->
                        searchedShipmentId = text
                    },
                    modifier = Modifier.weight(1f)
                )
                Spacer(modifier = Modifier.width(16.dp))
                Button(onClick = {
                    if (searchedShipmentId.isNotBlank()) {
                        val shipment = trackingSimulator.findShipment(searchedShipmentId)
                        if (shipment == null) {
                            snackbarVisible = true
                        } else {
                            trackerViewHelper.startTracking(shipment)
                        }
                        searchedShipmentId = ""
                    }
                }) {
                    Text(text = "Track")
                }
            }

            Column (
                Modifier
                    .verticalScroll(
                        rememberScrollState()
                    )
            ) {
                if (trackerViewHelper.shipments.isNotEmpty()) {
                    for(shipment in trackerViewHelper.shipments) {
                        TrackingCard(shipment, trackerViewHelper)
                    }
                }
                else {
                    Text(text = "Tracking none")
                }
            }

            if (snackbarVisible) {
                Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.BottomCenter) {
                    Snackbar(
                        modifier = Modifier.padding(16.dp)
                    ) {
                        Text(text = "Shipment not found!")
                    }
                }
                LaunchedEffect(snackbarVisible) {
                    delay(3000)
                    snackbarVisible = false
                }
            }
        }
    }
}

@Composable
fun TrackingCard(shipment: Shipment, trackerViewHelper: TrackerViewHelper) {
    Card(
        backgroundColor = Color.LightGray,
        border = BorderStroke(1.dp, Color.Black),
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 8.dp, horizontal = 16.dp)
    ) {
        Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.SpaceBetween,
            modifier = Modifier.fillMaxWidth()
        ) {
            Column(
                modifier = Modifier.padding(16.dp)
            ) {
                Text(
                    fontSize = 20.sp,
                    fontWeight = FontWeight.Bold,
                    text = "Tracking shipment: ${shipment.id}"
                )
                Spacer(modifier = Modifier.height(4.dp))
                if (shipment.updateHistory.isNotEmpty()) {
                    Text(text = "Status: " + shipment.updateHistory.last().newStatus)
                }
                else {
                    Text(text = "Status: None")
                }
                Spacer(modifier = Modifier.height(4.dp))
                if (shipment.locationHistory.isNotEmpty()) {
                    Text(text = "Location: " + shipment.locationHistory.last())
                }
                else {
                    Text(text = "Location: None")
                }
                Spacer(modifier = Modifier.height(4.dp))
                if (shipment.expectedDeliveryDateTimestampHistory.isNotEmpty()) {
                    Text(text = "Expected Delivery: " + Date(shipment.expectedDeliveryDateTimestampHistory.last()))
                }
                else {
                    Text(text = "Expected Delivery: None")
                }
                Spacer(modifier = Modifier.height(10.dp))
                Text(text = "Status Updates:")
                if (shipment.updateHistory.size > 0) {
                    Column {
                        for (shippingUpdate in shipment.updateHistory) {
                            Text(text = shippingUpdate.toString())
                        }
                    }
                }
                else {
                    Text(text = "None")
                }
                Spacer(modifier = Modifier.height(10.dp))
                Text(text = "Notes:")
                if (shipment.notes.size > 0) {
                    Column {
                        for (note in shipment.notes) {
                            Text(text = note)
                        }
                    }
                }
                else {
                    Text(text = "None")
                }
            }
            Box(
                modifier = Modifier
                    .padding(end = 8.dp)
                    .align(Alignment.Top)
                    .clickable {
                        trackerViewHelper.stopTracking(shipment)
                    }
            ) {
                Text(
                    text = "x",
                    color = Color.Blue,
                    modifier = Modifier.padding(8.dp)
                )
            }
        }
    }
}
