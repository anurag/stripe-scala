package org.apache.http.client.methods;

import java.net.URI;

import org.apache.http.annotation.NotThreadSafe;

public class HttpDeleteWithBody extends HttpEntityEnclosingRequestBase {
  String METHOD_NAME = "DELETE";

  public HttpDeleteWithBody() {
      super();
  }

  public HttpDeleteWithBody(final URI uri) {
      super();
      setURI(uri);
  }

  /**
   * @throws IllegalArgumentException if the uri is invalid.
   */
  public HttpDeleteWithBody(final String uri) {
      super();
      setURI(URI.create(uri));
  }

  @Override
  public String getMethod() {
      return METHOD_NAME;
  }
}
