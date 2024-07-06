import androidx.compose.desktop.ui.tooling.preview.Preview
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
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.Window
import androidx.compose.ui.window.application

@Composable
@Preview
fun App() {
    var shipmentId by remember { mutableStateOf("") }
    var shipmentIds by remember { mutableStateOf(listOf<String>()) }

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
                    value = shipmentId,
                    onValueChange = { text ->
                        shipmentId = text
                    },
                    modifier = Modifier.weight(1f)
                )
                Spacer(modifier = Modifier.width(16.dp))
                Button(onClick = {
                    if (shipmentId.isNotBlank()) {
                        shipmentIds = shipmentIds + shipmentId
                        shipmentId = ""
                    }
                }) {
                    Text(text = "Track")
                }
            }
            LazyColumn {
                items(shipmentIds) { shipmentId ->
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
                            Text(
                                text = shipmentId,
                                modifier = Modifier.weight(1f)
                            )
                            Box(
                                modifier = Modifier
                                    .padding(end = 8.dp)
                                    .align(Alignment.Top)
                                    .clickable {
                                        shipmentIds = shipmentIds.filter { it != shipmentId }
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
            }
        }
    }
}

fun main() = application {
    Window(onCloseRequest = ::exitApplication) {
        App()
    }
}
