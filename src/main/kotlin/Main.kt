import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Window
import androidx.compose.ui.window.application
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import listener.FileReader
import listener.Queue
import observer.TrackerViewHelper
import subject.Shipment
import subject.update.*
import java.util.*

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
const val fileName = "data/test.txt"
const val delimiter = ","
const val waitTimeMills = 1000L

val queue: Queue<String> = Queue()
val trackingSimulator = TrackingSimulator(typeToUpdateConstructor, delimiter, waitTimeMills, queue)
var shipmentIds by remember { mutableStateOf(listOf<String>()) }
val trackerViewHelper = TrackerViewHelper()

fun main() = runBlocking {
    launch {
        val fileReader = FileReader(queue, fileName)
        fileReader.listen()
    }
    launch {
        trackingSimulator.run()
    }
    application {
        Window(onCloseRequest = ::exitApplication) {
            App()
        }
    }
}

@Composable
fun App() {
    var searchedShipmentId by remember { mutableStateOf("") }

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
                        shipmentIds = shipmentIds + searchedShipmentId
                        val shipment = trackingSimulator.findShipment(searchedShipmentId)
                        if (shipment == null) {
                            // TODO notify the user they tried to track a shipment that doesnt exist
                        }
                        else {
                            trackerViewHelper.startTracking(shipment)
                        }
                        searchedShipmentId = ""
                    }
                }) {
                    Text(text = "Track")
                }
            }
            LazyColumn {
                items(shipmentIds) { shipmentId ->
                    TrackingCard(shipmentId)
                }
            }
        }
    }
}

@Composable
fun TrackingCard(shipmentId: String) {
    val shipment: Shipment = trackingSimulator.findShipment(shipmentId)!!
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
                Text(text = "Status: " + shipment.updateHistory[shipment.updateHistory.size-1].newStatus)
                Spacer(modifier = Modifier.height(4.dp))
                Text(text = "Location: " + shipment.locationHistory[shipment.locationHistory.size-1])
                Spacer(modifier = Modifier.height(4.dp))
                Text(text = "Expected Delivery: " + Date(shipment.expectedDeliveryDateTimestampHistory[shipment.expectedDeliveryDateTimestampHistory.size-1]))
                Spacer(modifier = Modifier.height(10.dp))
                Text(text = "Status Updates:")
                for (shippingUpdate in shipment.updateHistory) {
                    Text(text = "Shipment went from " + shippingUpdate.previousStatus + " to " + shippingUpdate.newStatus + " at " + Date(shippingUpdate.timestamp))
                }
                Spacer(modifier = Modifier.height(10.dp))
                Text(text = "Notes:")
                for (note in shipment.notes) {
                    Text(text = note)
                }
            }
            Box(
                modifier = Modifier
                    .padding(end = 8.dp)
                    .align(Alignment.Top)
                    .clickable {
                        shipmentIds = shipmentIds.filter { it != shipment.id }
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