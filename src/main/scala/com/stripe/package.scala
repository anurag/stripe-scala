package com

import org.apache.http.conn.ClientConnectionManager

package object stripe {
    var apiKey: String = ""
    var apiVersion: Option[String] = None
    var connectionManager: ClientConnectionManager = null
}
