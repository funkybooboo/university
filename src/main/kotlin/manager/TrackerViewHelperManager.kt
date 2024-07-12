package manager

import androidx.compose.runtime.remember
import observer.TrackerViewHelper

object TrackerViewHelperManager {
    val trackerViewHelper = remember { TrackerViewHelper() }
}