import mimetypes
from http.server import HTTPServer, BaseHTTPRequestHandler

class MyHTTPRequestHandler(BaseHTTPRequestHandler):
    """
    Your task is to define this class such that it fulfills the assingment
    requirements.

    Refer to the official Python documentation for the `http.server` class for
    details on what can go in here.

    Replace this pass statement with your own code:
    """

    def do_GET(self):
        if self.path == "/index.html" or self.path == "/about.html" or self.path == "/techtips+css.html" or self.path == "/techtips-css.html" or self.path == "/plan.html" or self.path == "/style.css":
            self.sendNormal(self.path[1:], "")
        elif "pictures" in self.path:
            extension = self.path.split(".")[1]
            self.sendNormal(self.path[1:], "image/"+extension)
        elif self.path == "" or self.path == "/" or self.path == "/index":
            self.redirect("index.html")
        elif self.path == "/about" or self.path == "/bio" or self.path == "/bio.html" or self.path == "/bioo" or self.path == "/biooo" or self.path == "/bioooo" or self.path == "/biography" or self.path == "/biopic" or self.path == "/biohazard" or self.path == "/biology" or self.path == "/biopsy":
            self.redirect("about.html")
        elif self.path == "/plan":
            self.redirect("plan.html")
        elif self.path == "/techtips+css" or self.path == "/tips" or self.path == "/help":
            self.redirect("techtips+css.html")
        elif self.path == "/techtips-css":
            self.redirect("techtips-css.html")
        elif self.path == "/debugging" or self.path == "/debugging.html":
            self.sendDebuggingContent(self.path)
        elif self.path == "/teapot" or self.path == "/teapot.html":
            self.sendNotTeaPot(self.path)
        elif self.path == "/forbidden" or self.path == "/topsecret.html" or self.path == "/topsecret" or self.path == "/personalinformation" or self.path == "/personalinformation.html":
            self.sendForbidden(self.path[1:])
        else:
            self.sendNormal(self.path[1:], "")

    def redirect(self, path):
        b = open(path, "rb").read()
        type = mimetypes.guess_type(path)[0]
        if (type == None):
            type = "text/html"
        self.send_response(301)
        self.send_header("Server", self.address_string())
        self.send_header("Date", self.date_time_string())
        self.send_header("Connection", "close")
        self.send_header("Cache-Control", "max-age=5")
        self.send_header("Content-Length", str(len(b)))
        self.send_header("Content-type", type)
        self.send_header("Location", path)
        self.end_headers()
        self.wfile.write(b)

    def sendNormal(self, path, fileType):
        try:
            b = open(path, "rb").read()
            response = 200
        except:
            page = ""
            page += "<h1>Page not found</h1>"
            page = self.makeHTMLConect(page, "Page not found")
            response = 404
            b = bytes(page, "utf-8")
        self.sendData(b, response, path, fileType)

    def sendData(self, b, response, path, fileType):
        if fileType == "":
            type = mimetypes.guess_type(path)[0]
        else:
            type = fileType
        if (type == None):
            type = "text/html"
        self.send_response(response)
        self.send_header("Server", self.address_string())
        self.send_header("Date", self.date_time_string())
        self.send_header("Connection", "close")
        self.send_header("Cache-Control", "max-age=5")
        self.send_header("Content-Length", str(len(b)))
        self.send_header("Content-type", type)
        self.end_headers()
        self.wfile.write(b)
        self.wfile.flush()

    def sendForbidden(self, path):
        page = ""
        page += "<p>Forbidden: You dont have access to this page</p>"
        page += "<p>"+path+"</p>"
        page = self.makeHTMLConect(page, "Forbidden")
        b = bytes(page, "utf-8")
        self.sendData(b, 403, path, "")

    def makeHTMLConect(self, content, title):
        page = ""
        page += "<!DOCTYPE html>"
        page += "<html lang=\"en\" class=\"html\">"
        page += "<head>"
        page += "<link rel=\"stylesheet\" href=\"style.css\">"
        page += "<link rel=\"icon\" href=\"favicon.ico\">"
        page += "<meta charset=\"UTF-8\">"
        page += "<title>"+title+"</title>"
        page += "</head>"
        page += "<body>"
        page += content
        page += "</body>"
        page += "</html>"
        return page

    def sendNotTeaPot(self, path):
        page = ""
        page += "<p>I'm not a teapot</p>"
        page += "<span><a href=\"index.html\">Main page</a></span>"
        page = self.makeHTMLConect(page, "Not a tea pot")
        b = bytes(page, "utf-8")
        self.sendData(b, 418, path, "")

    def sendDebuggingContent(self, path):
        page = ""
        page += "<div class=\"link\">"
        page += "<span><a href=\"index.html\">Main page</a></span>"
        page += "<span><a href=\"about.html\">About Me</a></span>"
        page += "<span><a href=\"techtips+css.html\">Tech Tips with CSS</a></span>"
        page += "<span><a href=\"techtips-css.html\">Tech Tips without CSS</a></span>"
        page += "<span><a href=\"debugging.html\">Server Debugging Page</a></span>"
        page += "<span><a href=\"plan.html\">Software Development Plan</a></span>"
        page += "</div>"
        page += "<h1>Server Debugging Page</h1>"
        page += "<h3>Use the following information to help debug the server</h3>"
        page += "<p>Server's Version: "+self.version_string()+"</p>"
        page += "<p>Server's Current date and time: "+self.date_time_string()+"</p>"
        page += "<p>Client's IP address and port number as observed by the server: "+str(self.client_address[0])+":"+str(self.client_address[1])+"</p>"
        page += "<p>Path requested by the client: "+self.path+"</p>"
        page += "<p>HTTP request type: "+self.command+"</p>"
        page += "<p>Version of this request: "+self.request_version+"</p>"
        page += "<p>Ordered list of the HTTP request headers sent by the browser:</p>"
        page += "<ol>"
        for header, value in self.headers.items():
            page += "<li>" + f"{header}: {value}" + "</li>"
        page += "</ol>"
        page = self.makeHTMLConect(page, "Server Debugging")
        b = bytes(page, "utf-8")
        self.sendData(b, 200, path, "")

if __name__ == '__main__':
    server_address = ('localhost', 8000)
    print(f"Serving from http://{server_address[0]}:{server_address[1]}")
    print("Press Ctrl-C to quit\n")
    try:
        HTTPServer(server_address, MyHTTPRequestHandler).serve_forever()
    except KeyboardInterrupt:
        print(" Exiting")
        exit(0)
