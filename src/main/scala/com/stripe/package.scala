package com

import org.apache.http.conn.ClientConnectionManager

package object stripe {
    var apiKey: String = ""
    var connectionManager: ClientConnectionManager = null
}
