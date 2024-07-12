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
import logger.Logger.Level
import manager.FileReaderManager.fileReader
import manager.LoggerManager.logger
import manager.TrackerViewHelperManager.trackerViewHelper
import manager.TrackingSimulatorManager.trackingSimulator
import subject.Shipment
import java.util.*

fun main() = runBlocking {
    launch {
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
            App()
        }
    }
}

@Composable
fun App() {
    var searchedShipmentId by remember { mutableStateOf("") }
    var snackbarVisible by remember { mutableStateOf(false) }

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
                    for(shipment in trackerViewHelper.shipments.values) {
                        TrackingCard(shipment)
                        println("hello")
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
fun TrackingCard(shipment: Shipment) {
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

                        trackingSimulator.findShipment(shipment.id)?.let { trackerViewHelper.stopTracking(it) }
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
