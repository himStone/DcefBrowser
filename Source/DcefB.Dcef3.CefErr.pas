(*
  *                  Delphi Multi-tab Chromium Browser Frame
  *
  * Usage allowed under the restrictions of the Lesser GNU General Public License
  * or alternatively the restrictions of the Mozilla Public License 1.1
  *
  * Software distributed under the License is distributed on an "AS IS" basis,
  * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  * the specific language governing rights and limitations under the License.
  *
  * Unit owner : BccSafe <bccsafe5988@gmail.com>
  * QQ         : 1262807955
  * Web site   : http://www.bccsafe.com
  * Repository : https://github.com/bccsafe/DcefBrowser
  *
  * The code of DcefBrowser is based on DCEF3 by: Henri Gourvest <hgourvest@gmail.com>
  * code: https://github.com/hgourvest/dcef3
  *
  * Embarcadero Technologies, Inc is not permitted to use or redistribute
  * this source code without explicit permission.
  *
*)

unit DcefB.Dcef3.CefErr;

(*
  This file contains the list of network errors.

  Ranges:
  0- 99 System related errors
  100-199 Connection related errors
  200-299 Certificate errors
  300-399 HTTP errors
  400-499 Cache errors
  500-599 ?
  600-699 FTP errors
  700-799 Certificate manager errors
  800-899 DNS resolver errors
*)

interface

uses
  Generics.Collections, Classes;

type
  TCefErrorManager = class
  private
    FErrorData: TDictionary<integer, TStrings>;
    procedure InitData();
  public
    constructor Create();
    destructor Destroy(); override;

    function IsUserAborted(const aErrCode: integer): Boolean;
    function GetErrorDescription(const aErrCode: integer;
      var aErrorName, aErrorDescription: string): Boolean;
  end;

const
NET_ERROR_IO_PENDING_ID = -1;
NET_ERROR_IO_PENDING_Name = 'NET_ERROR_IO_PENDING';
NET_ERROR_IO_PENDING_Des = 'An asynchronous IO operation is not yet complete.  This usually does not' + #13 + 'indicate a fatal error.  Typically this error will be generated as a' + #13 + 'notification to wait for some external notification that the IO operation' + #13 + 'finally completed.';

NET_ERROR_FAILED_ID = -2;
NET_ERROR_FAILED_Name = 'NET_ERROR_FAILED';
NET_ERROR_FAILED_Des = 'A generic failure occurred.';

NET_ERROR_ABORTED_ID = -3;
NET_ERROR_ABORTED_Name = 'NET_ERROR_ABORTED';
NET_ERROR_ABORTED_Des = 'An operation was aborted (due to user action).';

NET_ERROR_INVALID_ARGUMENT_ID = -4;
NET_ERROR_INVALID_ARGUMENT_Name = 'NET_ERROR_INVALID_ARGUMENT';
NET_ERROR_INVALID_ARGUMENT_Des = 'An argument to the function is incorrect.';

NET_ERROR_INVALID_HANDLE_ID = -5;
NET_ERROR_INVALID_HANDLE_Name = 'NET_ERROR_INVALID_HANDLE';
NET_ERROR_INVALID_HANDLE_Des = 'The handle or file descriptor is invalid.';

NET_ERROR_FILE_NOT_FOUND_ID = -6;
NET_ERROR_FILE_NOT_FOUND_Name = 'NET_ERROR_FILE_NOT_FOUND';
NET_ERROR_FILE_NOT_FOUND_Des = 'The file or directory cannot be found.';

NET_ERROR_TIMED_OUT_ID = -7;
NET_ERROR_TIMED_OUT_Name = 'NET_ERROR_TIMED_OUT';
NET_ERROR_TIMED_OUT_Des = 'An operation timed out.';

NET_ERROR_FILE_TOO_BIG_ID = -8;
NET_ERROR_FILE_TOO_BIG_Name = 'NET_ERROR_FILE_TOO_BIG';
NET_ERROR_FILE_TOO_BIG_Des = 'The file is too large.';

NET_ERROR_UNEXPECTED_ID = -9;
NET_ERROR_UNEXPECTED_Name = 'NET_ERROR_UNEXPECTED';
NET_ERROR_UNEXPECTED_Des = 'An unexpected error.  This may be caused by a programming mistake or an' + #13 + 'invalid assumption.';

NET_ERROR_ACCESS_DENIED_ID = -10;
NET_ERROR_ACCESS_DENIED_Name = 'NET_ERROR_ACCESS_DENIED';
NET_ERROR_ACCESS_DENIED_Des = 'Permission to access a resource, other than the network, was denied.';

NET_ERROR_NOT_IMPLEMENTED_ID = -11;
NET_ERROR_NOT_IMPLEMENTED_Name = 'NET_ERROR_NOT_IMPLEMENTED';
NET_ERROR_NOT_IMPLEMENTED_Des = 'The operation failed because of unimplemented functionality.';

NET_ERROR_INSUFFICIENT_RESOURCES_ID = -12;
NET_ERROR_INSUFFICIENT_RESOURCES_Name = 'NET_ERROR_INSUFFICIENT_RESOURCES';
NET_ERROR_INSUFFICIENT_RESOURCES_Des = 'There were not enough resources to complete the operation.';

NET_ERROR_OUT_OF_MEMORY_ID = -13;
NET_ERROR_OUT_OF_MEMORY_Name = 'NET_ERROR_OUT_OF_MEMORY';
NET_ERROR_OUT_OF_MEMORY_Des = 'Memory allocation failed.';

NET_ERROR_UPLOAD_FILE_CHANGED_ID = -14;
NET_ERROR_UPLOAD_FILE_CHANGED_Name = 'NET_ERROR_UPLOAD_FILE_CHANGED';
NET_ERROR_UPLOAD_FILE_CHANGED_Des = 'The file upload failed because the file''s modification time was different' + #13 + 'from the expectation.';

NET_ERROR_SOCKET_NOT_CONNECTED_ID = -15;
NET_ERROR_SOCKET_NOT_CONNECTED_Name = 'NET_ERROR_SOCKET_NOT_CONNECTED';
NET_ERROR_SOCKET_NOT_CONNECTED_Des = 'The socket is not connected.';

NET_ERROR_FILE_EXISTS_ID = -16;
NET_ERROR_FILE_EXISTS_Name = 'NET_ERROR_FILE_EXISTS';
NET_ERROR_FILE_EXISTS_Des = 'The file already exists.';

NET_ERROR_FILE_PATH_TOO_LONG_ID = -17;
NET_ERROR_FILE_PATH_TOO_LONG_Name = 'NET_ERROR_FILE_PATH_TOO_LONG';
NET_ERROR_FILE_PATH_TOO_LONG_Des = 'The path or file name is too long.';

NET_ERROR_FILE_NO_SPACE_ID = -18;
NET_ERROR_FILE_NO_SPACE_Name = 'NET_ERROR_FILE_NO_SPACE';
NET_ERROR_FILE_NO_SPACE_Des = 'Not enough room left on the disk.';

NET_ERROR_FILE_VIRUS_INFECTED_ID = -19;
NET_ERROR_FILE_VIRUS_INFECTED_Name = 'NET_ERROR_FILE_VIRUS_INFECTED';
NET_ERROR_FILE_VIRUS_INFECTED_Des = 'The file has a virus.';

NET_ERROR_BLOCKED_BY_CLIENT_ID = -20;
NET_ERROR_BLOCKED_BY_CLIENT_Name = 'NET_ERROR_BLOCKED_BY_CLIENT';
NET_ERROR_BLOCKED_BY_CLIENT_Des = 'The client chose to block the request.';

NET_ERROR_NETWORK_CHANGED_ID = -21;
NET_ERROR_NETWORK_CHANGED_Name = 'NET_ERROR_NETWORK_CHANGED';
NET_ERROR_NETWORK_CHANGED_Des = 'The network changed.';

NET_ERROR_BLOCKED_BY_ADMINISTRATOR_ID = -22;
NET_ERROR_BLOCKED_BY_ADMINISTRATOR_Name = 'NET_ERROR_BLOCKED_BY_ADMINISTRATOR';
NET_ERROR_BLOCKED_BY_ADMINISTRATOR_Des = 'The request was blocked by the URL blacklist configured by the domain' + #13 + 'administrator.';

NET_ERROR_CONNECTION_CLOSED_ID = -100;
NET_ERROR_CONNECTION_CLOSED_Name = 'NET_ERROR_CONNECTION_CLOSED';
NET_ERROR_CONNECTION_CLOSED_Des = 'A connection was closed (corresponding to a TCP FIN).';

NET_ERROR_CONNECTION_RESET_ID = -101;
NET_ERROR_CONNECTION_RESET_Name = 'NET_ERROR_CONNECTION_RESET';
NET_ERROR_CONNECTION_RESET_Des = 'A connection was reset (corresponding to a TCP RST).';

NET_ERROR_CONNECTION_REFUSED_ID = -102;
NET_ERROR_CONNECTION_REFUSED_Name = 'NET_ERROR_CONNECTION_REFUSED';
NET_ERROR_CONNECTION_REFUSED_Des = 'A connection attempt was refused.';

NET_ERROR_CONNECTION_ABORTED_ID = -103;
NET_ERROR_CONNECTION_ABORTED_Name = 'NET_ERROR_CONNECTION_ABORTED';
NET_ERROR_CONNECTION_ABORTED_Des = 'A connection timed out as a result of not receiving an ACK for data sent.' + #13 + 'This can include a FIN packet that did not get ACK''d.';

NET_ERROR_CONNECTION_FAILED_ID = -104;
NET_ERROR_CONNECTION_FAILED_Name = 'NET_ERROR_CONNECTION_FAILED';
NET_ERROR_CONNECTION_FAILED_Des = 'A connection attempt failed.';

NET_ERROR_NAME_NOT_RESOLVED_ID = -105;
NET_ERROR_NAME_NOT_RESOLVED_Name = 'NET_ERROR_NAME_NOT_RESOLVED';
NET_ERROR_NAME_NOT_RESOLVED_Des = 'The host name could not be resolved.';

NET_ERROR_INTERNET_DISCONNECTED_ID = -106;
NET_ERROR_INTERNET_DISCONNECTED_Name = 'NET_ERROR_INTERNET_DISCONNECTED';
NET_ERROR_INTERNET_DISCONNECTED_Des = 'The Internet connection has been lost.';

NET_ERROR_SSL_PROTOCOL_ERROR_ID = -107;
NET_ERROR_SSL_PROTOCOL_ERROR_Name = 'NET_ERROR_SSL_PROTOCOL_ERROR';
NET_ERROR_SSL_PROTOCOL_ERROR_Des = 'An SSL protocol error occurred.';

NET_ERROR_ADDRESS_INVALID_ID = -108;
NET_ERROR_ADDRESS_INVALID_Name = 'NET_ERROR_ADDRESS_INVALID';
NET_ERROR_ADDRESS_INVALID_Des = 'The IP address or port number is invalid (e.g., cannot connect to the IP' + #13 + 'address 0 or the port 0).';

NET_ERROR_ADDRESS_UNREACHABLE_ID = -109;
NET_ERROR_ADDRESS_UNREACHABLE_Name = 'NET_ERROR_ADDRESS_UNREACHABLE';
NET_ERROR_ADDRESS_UNREACHABLE_Des = 'The IP address is unreachable.  This usually means that there is no route to' + #13 + 'the specified host or network.';

NET_ERROR_SSL_CLIENT_AUTH_CERT_NEEDED_ID = -110;
NET_ERROR_SSL_CLIENT_AUTH_CERT_NEEDED_Name = 'NET_ERROR_SSL_CLIENT_AUTH_CERT_NEEDED';
NET_ERROR_SSL_CLIENT_AUTH_CERT_NEEDED_Des = 'The server requested a client certificate for SSL client authentication.';

NET_ERROR_TUNNEL_CONNECTION_FAILED_ID = -111;
NET_ERROR_TUNNEL_CONNECTION_FAILED_Name = 'NET_ERROR_TUNNEL_CONNECTION_FAILED';
NET_ERROR_TUNNEL_CONNECTION_FAILED_Des = 'A tunnel connection through the proxy could not be established.';

NET_ERROR_NO_SSL_VERSIONS_ENABLED_ID = -112;
NET_ERROR_NO_SSL_VERSIONS_ENABLED_Name = 'NET_ERROR_NO_SSL_VERSIONS_ENABLED';
NET_ERROR_NO_SSL_VERSIONS_ENABLED_Des = 'No SSL protocol versions are enabled.';

NET_ERROR_SSL_VERSION_OR_CIPHER_MISMATCH_ID = -113;
NET_ERROR_SSL_VERSION_OR_CIPHER_MISMATCH_Name = 'NET_ERROR_SSL_VERSION_OR_CIPHER_MISMATCH';
NET_ERROR_SSL_VERSION_OR_CIPHER_MISMATCH_Des = 'The client and server don''t support a common SSL protocol version or' + #13 + 'cipher suite.';

NET_ERROR_SSL_RENEGOTIATION_REQUESTED_ID = -114;
NET_ERROR_SSL_RENEGOTIATION_REQUESTED_Name = 'NET_ERROR_SSL_RENEGOTIATION_REQUESTED';
NET_ERROR_SSL_RENEGOTIATION_REQUESTED_Des = 'The server requested a renegotiation (rehandshake).';

NET_ERROR_PROXY_AUTH_UNSUPPORTED_ID = -115;
NET_ERROR_PROXY_AUTH_UNSUPPORTED_Name = 'NET_ERROR_PROXY_AUTH_UNSUPPORTED';
NET_ERROR_PROXY_AUTH_UNSUPPORTED_Des = 'The proxy requested authentication (for tunnel establishment) with an' + #13 + 'unsupported method.';

NET_ERROR_CERT_ERROR_IN_SSL_RENEGOTIATION_ID = -116;
NET_ERROR_CERT_ERROR_IN_SSL_RENEGOTIATION_Name = 'NET_ERROR_CERT_ERROR_IN_SSL_RENEGOTIATION';
NET_ERROR_CERT_ERROR_IN_SSL_RENEGOTIATION_Des = 'During SSL renegotiation (rehandshake), the server sent a certificate with' + #13 + 'an error.' + #13 + 'Note: this error is not in the -2xx range so that it won''t be handled as a' + #13 + 'certificate error.';

NET_ERROR_BAD_SSL_CLIENT_AUTH_CERT_ID = -117;
NET_ERROR_BAD_SSL_CLIENT_AUTH_CERT_Name = 'NET_ERROR_BAD_SSL_CLIENT_AUTH_CERT';
NET_ERROR_BAD_SSL_CLIENT_AUTH_CERT_Des = 'The SSL handshake failed because of a bad or missing client certificate.';

NET_ERROR_CONNECTION_TIMED_OUT_ID = -118;
NET_ERROR_CONNECTION_TIMED_OUT_Name = 'NET_ERROR_CONNECTION_TIMED_OUT';
NET_ERROR_CONNECTION_TIMED_OUT_Des = 'A connection attempt timed out.';

NET_ERROR_HOST_RESOLVER_QUEUE_TOO_LARGE_ID = -119;
NET_ERROR_HOST_RESOLVER_QUEUE_TOO_LARGE_Name = 'NET_ERROR_HOST_RESOLVER_QUEUE_TOO_LARGE';
NET_ERROR_HOST_RESOLVER_QUEUE_TOO_LARGE_Des = 'There are too many pending DNS resolves, so a request in the queue was' + #13 + 'aborted.';

NET_ERROR_SOCKS_CONNECTION_FAILED_ID = -120;
NET_ERROR_SOCKS_CONNECTION_FAILED_Name = 'NET_ERROR_SOCKS_CONNECTION_FAILED';
NET_ERROR_SOCKS_CONNECTION_FAILED_Des = 'Failed establishing a connection to the SOCKS proxy server for a target host.';

NET_ERROR_SOCKS_CONNECTION_HOST_UNREACHABLE_ID = -121;
NET_ERROR_SOCKS_CONNECTION_HOST_UNREACHABLE_Name = 'NET_ERROR_SOCKS_CONNECTION_HOST_UNREACHABLE';
NET_ERROR_SOCKS_CONNECTION_HOST_UNREACHABLE_Des = 'The SOCKS proxy server failed establishing connection to the target host' + #13 + 'because that host is unreachable.';

NET_ERROR_NPN_NEGOTIATION_FAILED_ID = -122;
NET_ERROR_NPN_NEGOTIATION_FAILED_Name = 'NET_ERROR_NPN_NEGOTIATION_FAILED';
NET_ERROR_NPN_NEGOTIATION_FAILED_Des = 'The request to negotiate an alternate protocol failed.';

NET_ERROR_SSL_NO_RENEGOTIATION_ID = -123;
NET_ERROR_SSL_NO_RENEGOTIATION_Name = 'NET_ERROR_SSL_NO_RENEGOTIATION';
NET_ERROR_SSL_NO_RENEGOTIATION_Des = 'The peer sent an SSL no_renegotiation alert message.';

NET_ERROR_WINSOCK_UNEXPECTED_WRITTEN_BYTES_ID = -124;
NET_ERROR_WINSOCK_UNEXPECTED_WRITTEN_BYTES_Name = 'NET_ERROR_WINSOCK_UNEXPECTED_WRITTEN_BYTES';
NET_ERROR_WINSOCK_UNEXPECTED_WRITTEN_BYTES_Des = 'Winsock sometimes reports more data written than passed.  This is probably' + #13 + 'due to a broken LSP.';

NET_ERROR_SSL_DECOMPRESSION_FAILURE_ALERT_ID = -125;
NET_ERROR_SSL_DECOMPRESSION_FAILURE_ALERT_Name = 'NET_ERROR_SSL_DECOMPRESSION_FAILURE_ALERT';
NET_ERROR_SSL_DECOMPRESSION_FAILURE_ALERT_Des = 'An SSL peer sent us a fatal decompression_failure alert. This typically' + #13 + 'occurs when a peer selects DEFLATE compression in the mistaken belief that' + #13 + 'it supports it.';

NET_ERROR_SSL_BAD_RECORD_MAC_ALERT_ID = -126;
NET_ERROR_SSL_BAD_RECORD_MAC_ALERT_Name = 'NET_ERROR_SSL_BAD_RECORD_MAC_ALERT';
NET_ERROR_SSL_BAD_RECORD_MAC_ALERT_Des = 'An SSL peer sent us a fatal bad_record_mac alert. This has been observed' + #13 + 'from servers with buggy DEFLATE support.';

NET_ERROR_PROXY_AUTH_REQUESTED_ID = -127;
NET_ERROR_PROXY_AUTH_REQUESTED_Name = 'NET_ERROR_PROXY_AUTH_REQUESTED';
NET_ERROR_PROXY_AUTH_REQUESTED_Des = 'The proxy requested authentication (for tunnel establishment).';

NET_ERROR_SSL_UNSAFE_NEGOTIATION_ID = -128;
NET_ERROR_SSL_UNSAFE_NEGOTIATION_Name = 'NET_ERROR_SSL_UNSAFE_NEGOTIATION';
NET_ERROR_SSL_UNSAFE_NEGOTIATION_Des = 'A known TLS strict server didn''t offer the renegotiation extension.';

NET_ERROR_SSL_WEAK_SERVER_EPHEMERAL_DH_KEY_ID = -129;
NET_ERROR_SSL_WEAK_SERVER_EPHEMERAL_DH_KEY_Name = 'NET_ERROR_SSL_WEAK_SERVER_EPHEMERAL_DH_KEY';
NET_ERROR_SSL_WEAK_SERVER_EPHEMERAL_DH_KEY_Des = 'The SSL server attempted to use a weak ephemeral Diffie-Hellman key.';

NET_ERROR_PROXY_CONNECTION_FAILED_ID = -130;
NET_ERROR_PROXY_CONNECTION_FAILED_Name = 'NET_ERROR_PROXY_CONNECTION_FAILED';
NET_ERROR_PROXY_CONNECTION_FAILED_Des = 'Could not create a connection to the proxy server. An error occurred' + #13 + 'either in resolving its name, or in connecting a socket to it.' + #13 + 'Note that this does NOT include failures during the actual "CONNECT" method' + #13 + 'of an HTTP proxy.';

NET_ERROR_MANDATORY_PROXY_CONFIGURATION_FAILED_ID = -131;
NET_ERROR_MANDATORY_PROXY_CONFIGURATION_FAILED_Name = 'NET_ERROR_MANDATORY_PROXY_CONFIGURATION_FAILED';
NET_ERROR_MANDATORY_PROXY_CONFIGURATION_FAILED_Des = 'A mandatory proxy configuration could not be used. Currently this means' + #13 + 'that a mandatory PAC script could not be fetched, parsed or executed.';

NET_ERROR_PRECONNECT_MAX_SOCKET_LIMIT_ID = -133;
NET_ERROR_PRECONNECT_MAX_SOCKET_LIMIT_Name = 'NET_ERROR_PRECONNECT_MAX_SOCKET_LIMIT';
NET_ERROR_PRECONNECT_MAX_SOCKET_LIMIT_Des = '-132 was formerly ERR_ESET_ANTI_VIRUS_SSL_INTERCEPTION' + #13 + 'We''ve hit the max socket limit for the socket pool while preconnecting.  We' + #13 + 'don''t bother trying to preconnect more sockets.';

NET_ERROR_SSL_CLIENT_AUTH_PRIVATE_KEY_ACCESS_DENIED_ID = -134;
NET_ERROR_SSL_CLIENT_AUTH_PRIVATE_KEY_ACCESS_DENIED_Name = 'NET_ERROR_SSL_CLIENT_AUTH_PRIVATE_KEY_ACCESS_DENIED';
NET_ERROR_SSL_CLIENT_AUTH_PRIVATE_KEY_ACCESS_DENIED_Des = 'The permission to use the SSL client certificate''s private key was denied.';

NET_ERROR_SSL_CLIENT_AUTH_CERT_NO_PRIVATE_KEY_ID = -135;
NET_ERROR_SSL_CLIENT_AUTH_CERT_NO_PRIVATE_KEY_Name = 'NET_ERROR_SSL_CLIENT_AUTH_CERT_NO_PRIVATE_KEY';
NET_ERROR_SSL_CLIENT_AUTH_CERT_NO_PRIVATE_KEY_Des = 'The SSL client certificate has no private key.';

NET_ERROR_PROXY_CERTIFICATE_INVALID_ID = -136;
NET_ERROR_PROXY_CERTIFICATE_INVALID_Name = 'NET_ERROR_PROXY_CERTIFICATE_INVALID';
NET_ERROR_PROXY_CERTIFICATE_INVALID_Des = 'The certificate presented by the HTTPS Proxy was invalid.';

NET_ERROR_NAME_RESOLUTION_FAILED_ID = -137;
NET_ERROR_NAME_RESOLUTION_FAILED_Name = 'NET_ERROR_NAME_RESOLUTION_FAILED';
NET_ERROR_NAME_RESOLUTION_FAILED_Des = 'An error occurred when trying to do a name resolution (DNS).';

NET_ERROR_NETWORK_ACCESS_DENIED_ID = -138;
NET_ERROR_NETWORK_ACCESS_DENIED_Name = 'NET_ERROR_NETWORK_ACCESS_DENIED';
NET_ERROR_NETWORK_ACCESS_DENIED_Des = 'Permission to access the network was denied. This is used to distinguish' + #13 + 'errors that were most likely caused by a firewall from other access denied' + #13 + 'errors. See also ERR_ACCESS_DENIED.';

NET_ERROR_TEMPORARILY_THROTTLED_ID = -139;
NET_ERROR_TEMPORARILY_THROTTLED_Name = 'NET_ERROR_TEMPORARILY_THROTTLED';
NET_ERROR_TEMPORARILY_THROTTLED_Des = 'The request throttler module cancelled this request to avoid DDOS.';

NET_ERROR_HTTPS_PROXY_TUNNEL_RESPONSE_ID = -140;
NET_ERROR_HTTPS_PROXY_TUNNEL_RESPONSE_Name = 'NET_ERROR_HTTPS_PROXY_TUNNEL_RESPONSE';
NET_ERROR_HTTPS_PROXY_TUNNEL_RESPONSE_Des = 'A request to create an SSL tunnel connection through the HTTPS proxy' + #13 + 'received a non-200 (OK) and non-407 (Proxy Auth) response.  The response' + #13 + 'body might include a description of why the request failed.';

NET_ERROR_SSL_CLIENT_AUTH_SIGNATURE_FAILED_ID = -141;
NET_ERROR_SSL_CLIENT_AUTH_SIGNATURE_FAILED_Name = 'NET_ERROR_SSL_CLIENT_AUTH_SIGNATURE_FAILED';
NET_ERROR_SSL_CLIENT_AUTH_SIGNATURE_FAILED_Des = 'We were unable to sign the CertificateVerify data of an SSL client auth' + #13 + 'handshake with the client certificate''s private key.' + #13 + 'Possible causes for this include the user implicitly or explicitly' + #13 + 'denying access to the private key, the private key may not be valid for' + #13 + 'signing, the key may be relying on a cached handle which is no longer' + #13 + 'valid, or the CSP won''t allow arbitrary data to be signed.';

NET_ERROR_MSG_TOO_BIG_ID = -142;
NET_ERROR_MSG_TOO_BIG_Name = 'NET_ERROR_MSG_TOO_BIG';
NET_ERROR_MSG_TOO_BIG_Des = 'The message was too large for the transport.  (for example a UDP message' + #13 + 'which exceeds size threshold).';

NET_ERROR_SPDY_SESSION_ALREADY_EXISTS_ID = -143;
NET_ERROR_SPDY_SESSION_ALREADY_EXISTS_Name = 'NET_ERROR_SPDY_SESSION_ALREADY_EXISTS';
NET_ERROR_SPDY_SESSION_ALREADY_EXISTS_Des = 'A SPDY session already exists, and should be used instead of this connection.';

NET_ERROR_WS_PROTOCOL_ERROR_ID = -145;
NET_ERROR_WS_PROTOCOL_ERROR_Name = 'NET_ERROR_WS_PROTOCOL_ERROR';
NET_ERROR_WS_PROTOCOL_ERROR_Des = 'Error -144 was removed (LIMIT_VIOLATION).' + #13 + 'Websocket protocol error. Indicates that we are terminating the connection' + #13 + 'due to a malformed frame or other protocol violation.';

NET_ERROR_PROTOCOL_SWITCHED_ID = -146;
NET_ERROR_PROTOCOL_SWITCHED_Name = 'NET_ERROR_PROTOCOL_SWITCHED';
NET_ERROR_PROTOCOL_SWITCHED_Des = 'Connection was aborted for switching to another ptotocol.' + #13 + 'WebSocket abort SocketStream connection when alternate protocol is found.';

NET_ERROR_ADDRESS_IN_USE_ID = -147;
NET_ERROR_ADDRESS_IN_USE_Name = 'NET_ERROR_ADDRESS_IN_USE';
NET_ERROR_ADDRESS_IN_USE_Des = 'Returned when attempting to bind an address that is already in use.';

NET_ERROR_SSL_HANDSHAKE_NOT_COMPLETED_ID = -148;
NET_ERROR_SSL_HANDSHAKE_NOT_COMPLETED_Name = 'NET_ERROR_SSL_HANDSHAKE_NOT_COMPLETED';
NET_ERROR_SSL_HANDSHAKE_NOT_COMPLETED_Des = 'An operation failed because the SSL handshake has not completed.';

NET_ERROR_SSL_BAD_PEER_PUBLIC_KEY_ID = -149;
NET_ERROR_SSL_BAD_PEER_PUBLIC_KEY_Name = 'NET_ERROR_SSL_BAD_PEER_PUBLIC_KEY';
NET_ERROR_SSL_BAD_PEER_PUBLIC_KEY_Des = 'SSL peer''s public key is invalid.';

NET_ERROR_SSL_PINNED_KEY_NOT_IN_CERT_CHAIN_ID = -150;
NET_ERROR_SSL_PINNED_KEY_NOT_IN_CERT_CHAIN_Name = 'NET_ERROR_SSL_PINNED_KEY_NOT_IN_CERT_CHAIN';
NET_ERROR_SSL_PINNED_KEY_NOT_IN_CERT_CHAIN_Des = 'The certificate didn''t match the built-in public key pins for the host name.' + #13 + 'The pins are set in net/http/transport_security_state.cc and require that' + #13 + 'one of a set of public keys exist on the path from the leaf to the root.';

NET_ERROR_CLIENT_AUTH_CERT_TYPE_UNSUPPORTED_ID = -151;
NET_ERROR_CLIENT_AUTH_CERT_TYPE_UNSUPPORTED_Name = 'NET_ERROR_CLIENT_AUTH_CERT_TYPE_UNSUPPORTED';
NET_ERROR_CLIENT_AUTH_CERT_TYPE_UNSUPPORTED_Des = 'Server request for client certificate did not contain any types we support.';

NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_TYPE_MISMATCH_ID = -152;
NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_TYPE_MISMATCH_Name = 'NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_TYPE_MISMATCH';
NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_TYPE_MISMATCH_Des = 'Server requested one type of cert, then requested a different type while the' + #13 + 'first was still being generated.';

NET_ERROR_CERT_COMMON_NAME_INVALID_ID = -200;
NET_ERROR_CERT_COMMON_NAME_INVALID_Name = 'NET_ERROR_CERT_COMMON_NAME_INVALID';
NET_ERROR_CERT_COMMON_NAME_INVALID_Des = 'Certificate error codes' + #13 + 'The values of certificate error codes must be consecutive.' + #13 + 'The server responded with a certificate whose common name did not match' + #13 + 'the host name.  This could mean:' + #13 + '1. An attacker has redirected our traffic to his server and is' + #13 + '   presenting a certificate for which he knows the private key.' + #13 + '2. The server is misconfigured and responding with the wrong cert.' + #13 + '3. The user is on a wireless network and is being redirected to the' + #13 + '   network''s login page.' + #13 + '4. The OS has used a DNS search suffix and the server doesn''t have' + #13 + '   a certificate for the abbreviated name in the address bar.';

NET_ERROR_CERT_DATE_INVALID_ID = -201;
NET_ERROR_CERT_DATE_INVALID_Name = 'NET_ERROR_CERT_DATE_INVALID';
NET_ERROR_CERT_DATE_INVALID_Des = 'The server responded with a certificate that, by our clock, appears to' + #13 + 'either not yet be valid or to have expired.  This could mean:' + #13 + '1. An attacker is presenting an old certificate for which he has' + #13 + '   managed to obtain the private key.' + #13 + '2. The server is misconfigured and is not presenting a valid cert.' + #13 + '3. Our clock is wrong.';

NET_ERROR_CERT_AUTHORITY_INVALID_ID = -202;
NET_ERROR_CERT_AUTHORITY_INVALID_Name = 'NET_ERROR_CERT_AUTHORITY_INVALID';
NET_ERROR_CERT_AUTHORITY_INVALID_Des = 'The server responded with a certificate that is signed by an authority' + #13 + 'we don''t trust.  The could mean:' + #13 + '1. An attacker has substituted the real certificate for a cert that' + #13 + '   contains his public key and is signed by his cousin.' + #13 + '2. The server operator has a legitimate certificate from a CA we don''t' + #13 + '   know about, but should trust.' + #13 + '3. The server is presenting a self-signed certificate, providing no' + #13 + '   defense against active attackers (but foiling passive attackers).';

NET_ERROR_CERT_CONTAINS_ERRORS_ID = -203;
NET_ERROR_CERT_CONTAINS_ERRORS_Name = 'NET_ERROR_CERT_CONTAINS_ERRORS';
NET_ERROR_CERT_CONTAINS_ERRORS_Des = 'The server responded with a certificate that contains errors.' + #13 + 'This error is not recoverable.' + #13 + 'MSDN describes this error as follows:' + #13 + '  "The SSL certificate contains errors."' + #13 + 'NOTE: It''s unclear how this differs from ERR_CERT_INVALID. For consistency,' + #13 + 'use that code instead of this one from now on.';

NET_ERROR_CERT_NO_REVOCATION_MECHANISM_ID = -204;
NET_ERROR_CERT_NO_REVOCATION_MECHANISM_Name = 'NET_ERROR_CERT_NO_REVOCATION_MECHANISM';
NET_ERROR_CERT_NO_REVOCATION_MECHANISM_Des = 'The certificate has no mechanism for determining if it is revoked.  In' + #13 + 'effect, this certificate cannot be revoked.';

NET_ERROR_CERT_UNABLE_TO_CHECK_REVOCATION_ID = -205;
NET_ERROR_CERT_UNABLE_TO_CHECK_REVOCATION_Name = 'NET_ERROR_CERT_UNABLE_TO_CHECK_REVOCATION';
NET_ERROR_CERT_UNABLE_TO_CHECK_REVOCATION_Des = 'Revocation information for the security certificate for this site is not' + #13 + 'available.  This could mean:' + #13 + '1. An attacker has compromised the private key in the certificate and is' + #13 + '   blocking our attempt to find out that the cert was revoked.' + #13 + '2. The certificate is unrevoked, but the revocation server is busy or' + #13 + '   unavailable.';

NET_ERROR_CERT_REVOKED_ID = -206;
NET_ERROR_CERT_REVOKED_Name = 'NET_ERROR_CERT_REVOKED';
NET_ERROR_CERT_REVOKED_Des = 'The server responded with a certificate has been revoked.' + #13 + 'We have the capability to ignore this error, but it is probably not the' + #13 + 'thing to do.';

NET_ERROR_CERT_INVALID_ID = -207;
NET_ERROR_CERT_INVALID_Name = 'NET_ERROR_CERT_INVALID';
NET_ERROR_CERT_INVALID_Des = 'The server responded with a certificate that is invalid.' + #13 + 'This error is not recoverable.' + #13 + 'MSDN describes this error as follows:' + #13 + '  "The SSL certificate is invalid."';

NET_ERROR_CERT_WEAK_SIGNATURE_ALGORITHM_ID = -208;
NET_ERROR_CERT_WEAK_SIGNATURE_ALGORITHM_Name = 'NET_ERROR_CERT_WEAK_SIGNATURE_ALGORITHM';
NET_ERROR_CERT_WEAK_SIGNATURE_ALGORITHM_Des = 'The server responded with a certificate that is signed using a weak' + #13 + 'signature algorithm.';

NET_ERROR_CERT_NON_UNIQUE_NAME_ID = -210;
NET_ERROR_CERT_NON_UNIQUE_NAME_Name = 'NET_ERROR_CERT_NON_UNIQUE_NAME';
NET_ERROR_CERT_NON_UNIQUE_NAME_Des = '-209 is availible: was CERT_NOT_IN_DNS.' + #13 + 'The host name specified in the certificate is not unique.';

NET_ERROR_CERT_WEAK_KEY_ID = -211;
NET_ERROR_CERT_WEAK_KEY_Name = 'NET_ERROR_CERT_WEAK_KEY';
NET_ERROR_CERT_WEAK_KEY_Des = 'The server responded with a certificate that contains a weak key (e.g.' + #13 + 'a too-small RSA key).';

NET_ERROR_CERT_END_ID = -212;
NET_ERROR_CERT_END_Name = 'NET_ERROR_CERT_END';
NET_ERROR_CERT_END_Des = 'Add new certificate error codes here.' + #13 + 'Update the value of CERT_END whenever you add a new certificate error' + #13 + 'code.' + #13 + 'The value immediately past the last certificate error code.';

NET_ERROR_INVALID_URL_ID = -300;
NET_ERROR_INVALID_URL_Name = 'NET_ERROR_INVALID_URL';
NET_ERROR_INVALID_URL_Des = 'The URL is invalid.';

NET_ERROR_DISALLOWED_URL_SCHEME_ID = -301;
NET_ERROR_DISALLOWED_URL_SCHEME_Name = 'NET_ERROR_DISALLOWED_URL_SCHEME';
NET_ERROR_DISALLOWED_URL_SCHEME_Des = 'The scheme of the URL is disallowed.';

NET_ERROR_UNKNOWN_URL_SCHEME_ID = -302;
NET_ERROR_UNKNOWN_URL_SCHEME_Name = 'NET_ERROR_UNKNOWN_URL_SCHEME';
NET_ERROR_UNKNOWN_URL_SCHEME_Des = 'The scheme of the URL is unknown.';

NET_ERROR_TOO_MANY_REDIRECTS_ID = -310;
NET_ERROR_TOO_MANY_REDIRECTS_Name = 'NET_ERROR_TOO_MANY_REDIRECTS';
NET_ERROR_TOO_MANY_REDIRECTS_Des = 'Attempting to load an URL resulted in too many redirects.';

NET_ERROR_UNSAFE_REDIRECT_ID = -311;
NET_ERROR_UNSAFE_REDIRECT_Name = 'NET_ERROR_UNSAFE_REDIRECT';
NET_ERROR_UNSAFE_REDIRECT_Des = 'Attempting to load an URL resulted in an unsafe redirect (e.g., a redirect' + #13 + 'to file:// is considered unsafe).';

NET_ERROR_UNSAFE_PORT_ID = -312;
NET_ERROR_UNSAFE_PORT_Name = 'NET_ERROR_UNSAFE_PORT';
NET_ERROR_UNSAFE_PORT_Des = 'Attempting to load an URL with an unsafe port number.  These are port' + #13 + 'numbers that correspond to services, which are not robust to spurious input' + #13 + 'that may be constructed as a result of an allowed web construct (e.g., HTTP' + #13 + 'looks a lot like SMTP, so form submission to port 25 is denied).';

NET_ERROR_INVALID_RESPONSE_ID = -320;
NET_ERROR_INVALID_RESPONSE_Name = 'NET_ERROR_INVALID_RESPONSE';
NET_ERROR_INVALID_RESPONSE_Des = 'The server''s response was invalid.';

NET_ERROR_INVALID_CHUNKED_ENCODING_ID = -321;
NET_ERROR_INVALID_CHUNKED_ENCODING_Name = 'NET_ERROR_INVALID_CHUNKED_ENCODING';
NET_ERROR_INVALID_CHUNKED_ENCODING_Des = 'Error in chunked transfer encoding.';

NET_ERROR_METHOD_NOT_SUPPORTED_ID = -322;
NET_ERROR_METHOD_NOT_SUPPORTED_Name = 'NET_ERROR_METHOD_NOT_SUPPORTED';
NET_ERROR_METHOD_NOT_SUPPORTED_Des = 'The server did not support the request method.';

NET_ERROR_UNEXPECTED_PROXY_AUTH_ID = -323;
NET_ERROR_UNEXPECTED_PROXY_AUTH_Name = 'NET_ERROR_UNEXPECTED_PROXY_AUTH';
NET_ERROR_UNEXPECTED_PROXY_AUTH_Des = 'The response was 407 (Proxy Authentication Required), yet we did not send' + #13 + 'the request to a proxy.';

NET_ERROR_EMPTY_RESPONSE_ID = -324;
NET_ERROR_EMPTY_RESPONSE_Name = 'NET_ERROR_EMPTY_RESPONSE';
NET_ERROR_EMPTY_RESPONSE_Des = 'The server closed the connection without sending any data.';

NET_ERROR_RESPONSE_HEADERS_TOO_BIG_ID = -325;
NET_ERROR_RESPONSE_HEADERS_TOO_BIG_Name = 'NET_ERROR_RESPONSE_HEADERS_TOO_BIG';
NET_ERROR_RESPONSE_HEADERS_TOO_BIG_Des = 'The headers section of the response is too large.';

NET_ERROR_PAC_STATUS_NOT_OK_ID = -326;
NET_ERROR_PAC_STATUS_NOT_OK_Name = 'NET_ERROR_PAC_STATUS_NOT_OK';
NET_ERROR_PAC_STATUS_NOT_OK_Des = 'The PAC requested by HTTP did not have a valid status code (non-200).';

NET_ERROR_PAC_SCRIPT_FAILED_ID = -327;
NET_ERROR_PAC_SCRIPT_FAILED_Name = 'NET_ERROR_PAC_SCRIPT_FAILED';
NET_ERROR_PAC_SCRIPT_FAILED_Des = 'The evaluation of the PAC script failed.';

NET_ERROR_REQUEST_RANGE_NOT_SATISFIABLE_ID = -328;
NET_ERROR_REQUEST_RANGE_NOT_SATISFIABLE_Name = 'NET_ERROR_REQUEST_RANGE_NOT_SATISFIABLE';
NET_ERROR_REQUEST_RANGE_NOT_SATISFIABLE_Des = 'The response was 416 (Requested range not satisfiable) and the server cannot' + #13 + 'satisfy the range requested.';

NET_ERROR_MALFORMED_IDENTITY_ID = -329;
NET_ERROR_MALFORMED_IDENTITY_Name = 'NET_ERROR_MALFORMED_IDENTITY';
NET_ERROR_MALFORMED_IDENTITY_Des = 'The identity used for authentication is invalid.';

NET_ERROR_CONTENT_DECODING_FAILED_ID = -330;
NET_ERROR_CONTENT_DECODING_FAILED_Name = 'NET_ERROR_CONTENT_DECODING_FAILED';
NET_ERROR_CONTENT_DECODING_FAILED_Des = 'Content decoding of the response body failed.';

NET_ERROR_NETWORK_IO_SUSPENDED_ID = -331;
NET_ERROR_NETWORK_IO_SUSPENDED_Name = 'NET_ERROR_NETWORK_IO_SUSPENDED';
NET_ERROR_NETWORK_IO_SUSPENDED_Des = 'An operation could not be completed because all network IO' + #13 + 'is suspended.';

NET_ERROR_SYN_REPLY_NOT_RECEIVED_ID = -332;
NET_ERROR_SYN_REPLY_NOT_RECEIVED_Name = 'NET_ERROR_SYN_REPLY_NOT_RECEIVED';
NET_ERROR_SYN_REPLY_NOT_RECEIVED_Des = 'FLIP data received without receiving a SYN_REPLY on the stream.';

NET_ERROR_ENCODING_CONVERSION_FAILED_ID = -333;
NET_ERROR_ENCODING_CONVERSION_FAILED_Name = 'NET_ERROR_ENCODING_CONVERSION_FAILED';
NET_ERROR_ENCODING_CONVERSION_FAILED_Des = 'Converting the response to target encoding failed.';

NET_ERROR_UNRECOGNIZED_FTP_DIRECTORY_LISTING_FORMAT_ID = -334;
NET_ERROR_UNRECOGNIZED_FTP_DIRECTORY_LISTING_FORMAT_Name = 'NET_ERROR_UNRECOGNIZED_FTP_DIRECTORY_LISTING_FORMAT';
NET_ERROR_UNRECOGNIZED_FTP_DIRECTORY_LISTING_FORMAT_Des = 'The server sent an FTP directory listing in a format we do not understand.';

NET_ERROR_INVALID_SPDY_STREAM_ID = -335;
NET_ERROR_INVALID_SPDY_STREAM_Name = 'NET_ERROR_INVALID_SPDY_STREAM';
NET_ERROR_INVALID_SPDY_STREAM_Des = 'Attempted use of an unknown SPDY stream id.';

NET_ERROR_NO_SUPPORTED_PROXIES_ID = -336;
NET_ERROR_NO_SUPPORTED_PROXIES_Name = 'NET_ERROR_NO_SUPPORTED_PROXIES';
NET_ERROR_NO_SUPPORTED_PROXIES_Des = 'There are no supported proxies in the provided list.';

NET_ERROR_SPDY_PROTOCOL_ERROR_ID = -337;
NET_ERROR_SPDY_PROTOCOL_ERROR_Name = 'NET_ERROR_SPDY_PROTOCOL_ERROR';
NET_ERROR_SPDY_PROTOCOL_ERROR_Des = 'There is a SPDY protocol error.';

NET_ERROR_INVALID_AUTH_CREDENTIALS_ID = -338;
NET_ERROR_INVALID_AUTH_CREDENTIALS_Name = 'NET_ERROR_INVALID_AUTH_CREDENTIALS';
NET_ERROR_INVALID_AUTH_CREDENTIALS_Des = 'Credentials could not be established during HTTP Authentication.';

NET_ERROR_UNSUPPORTED_AUTH_SCHEME_ID = -339;
NET_ERROR_UNSUPPORTED_AUTH_SCHEME_Name = 'NET_ERROR_UNSUPPORTED_AUTH_SCHEME';
NET_ERROR_UNSUPPORTED_AUTH_SCHEME_Des = 'An HTTP Authentication scheme was tried which is not supported on this' + #13 + 'machine.';

NET_ERROR_ENCODING_DETECTION_FAILED_ID = -340;
NET_ERROR_ENCODING_DETECTION_FAILED_Name = 'NET_ERROR_ENCODING_DETECTION_FAILED';
NET_ERROR_ENCODING_DETECTION_FAILED_Des = 'Detecting the encoding of the response failed.';

NET_ERROR_MISSING_AUTH_CREDENTIALS_ID = -341;
NET_ERROR_MISSING_AUTH_CREDENTIALS_Name = 'NET_ERROR_MISSING_AUTH_CREDENTIALS';
NET_ERROR_MISSING_AUTH_CREDENTIALS_Des = '(GSSAPI) No Kerberos credentials were available during HTTP Authentication.';

NET_ERROR_UNEXPECTED_SECURITY_LIBRARY_STATUS_ID = -342;
NET_ERROR_UNEXPECTED_SECURITY_LIBRARY_STATUS_Name = 'NET_ERROR_UNEXPECTED_SECURITY_LIBRARY_STATUS';
NET_ERROR_UNEXPECTED_SECURITY_LIBRARY_STATUS_Des = 'An unexpected, but documented, SSPI or GSSAPI status code was returned.';

NET_ERROR_MISCONFIGURED_AUTH_ENVIRONMENT_ID = -343;
NET_ERROR_MISCONFIGURED_AUTH_ENVIRONMENT_Name = 'NET_ERROR_MISCONFIGURED_AUTH_ENVIRONMENT';
NET_ERROR_MISCONFIGURED_AUTH_ENVIRONMENT_Des = 'The environment was not set up correctly for authentication (for' + #13 + 'example, no KDC could be found or the principal is unknown.';

NET_ERROR_UNDOCUMENTED_SECURITY_LIBRARY_STATUS_ID = -344;
NET_ERROR_UNDOCUMENTED_SECURITY_LIBRARY_STATUS_Name = 'NET_ERROR_UNDOCUMENTED_SECURITY_LIBRARY_STATUS';
NET_ERROR_UNDOCUMENTED_SECURITY_LIBRARY_STATUS_Des = 'An undocumented SSPI or GSSAPI status code was returned.';

NET_ERROR_RESPONSE_BODY_TOO_BIG_TO_DRAIN_ID = -345;
NET_ERROR_RESPONSE_BODY_TOO_BIG_TO_DRAIN_Name = 'NET_ERROR_RESPONSE_BODY_TOO_BIG_TO_DRAIN';
NET_ERROR_RESPONSE_BODY_TOO_BIG_TO_DRAIN_Des = 'The HTTP response was too big to drain.';

NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_LENGTH_ID = -346;
NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_LENGTH_Name = 'NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_LENGTH';
NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_LENGTH_Des = 'The HTTP response contained multiple distinct Content-Length headers.';

NET_ERROR_INCOMPLETE_SPDY_HEADERS_ID = -347;
NET_ERROR_INCOMPLETE_SPDY_HEADERS_Name = 'NET_ERROR_INCOMPLETE_SPDY_HEADERS';
NET_ERROR_INCOMPLETE_SPDY_HEADERS_Des = 'SPDY Headers have been received, but not all of them - status or version' + #13 + 'headers are missing, so we''re expecting additional frames to complete them.';

NET_ERROR_PAC_NOT_IN_DHCP_ID = -348;
NET_ERROR_PAC_NOT_IN_DHCP_Name = 'NET_ERROR_PAC_NOT_IN_DHCP';
NET_ERROR_PAC_NOT_IN_DHCP_Des = 'No PAC URL configuration could be retrieved from DHCP. This can indicate' + #13 + 'either a failure to retrieve the DHCP configuration, or that there was no' + #13 + 'PAC URL configured in DHCP.';

NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_DISPOSITION_ID = -349;
NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_DISPOSITION_Name = 'NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_DISPOSITION';
NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_DISPOSITION_Des = 'The HTTP response contained multiple Content-Disposition headers.';

NET_ERROR_RESPONSE_HEADERS_MULTIPLE_LOCATION_ID = -350;
NET_ERROR_RESPONSE_HEADERS_MULTIPLE_LOCATION_Name = 'NET_ERROR_RESPONSE_HEADERS_MULTIPLE_LOCATION';
NET_ERROR_RESPONSE_HEADERS_MULTIPLE_LOCATION_Des = 'The HTTP response contained multiple Location headers.';

NET_ERROR_SPDY_SERVER_REFUSED_STREAM_ID = -351;
NET_ERROR_SPDY_SERVER_REFUSED_STREAM_Name = 'NET_ERROR_SPDY_SERVER_REFUSED_STREAM';
NET_ERROR_SPDY_SERVER_REFUSED_STREAM_Des = 'SPDY server refused the stream. Client should retry. This should never be a' + #13 + 'user-visible error.';

NET_ERROR_SPDY_PING_FAILED_ID = -352;
NET_ERROR_SPDY_PING_FAILED_Name = 'NET_ERROR_SPDY_PING_FAILED';
NET_ERROR_SPDY_PING_FAILED_Des = 'SPDY server didn''t respond to the PING message.';

NET_ERROR_PIPELINE_EVICTION_ID = -353;
NET_ERROR_PIPELINE_EVICTION_Name = 'NET_ERROR_PIPELINE_EVICTION';
NET_ERROR_PIPELINE_EVICTION_Des = 'The request couldn''t be completed on an HTTP pipeline. Client should retry.';

NET_ERROR_CONTENT_LENGTH_MISMATCH_ID = -354;
NET_ERROR_CONTENT_LENGTH_MISMATCH_Name = 'NET_ERROR_CONTENT_LENGTH_MISMATCH';
NET_ERROR_CONTENT_LENGTH_MISMATCH_Des = 'The HTTP response body transferred fewer bytes than were advertised by the' + #13 + 'Content-Length header when the connection is closed.';

NET_ERROR_INCOMPLETE_CHUNKED_ENCODING_ID = -355;
NET_ERROR_INCOMPLETE_CHUNKED_ENCODING_Name = 'NET_ERROR_INCOMPLETE_CHUNKED_ENCODING';
NET_ERROR_INCOMPLETE_CHUNKED_ENCODING_Des = 'The HTTP response body is transferred with Chunked-Encoding, but the' + #13 + 'terminating zero-length chunk was never sent when the connection is closed.';

NET_ERROR_QUIC_PROTOCOL_ERROR_ID = -356;
NET_ERROR_QUIC_PROTOCOL_ERROR_Name = 'NET_ERROR_QUIC_PROTOCOL_ERROR';
NET_ERROR_QUIC_PROTOCOL_ERROR_Des = 'There is a QUIC protocol error.';

NET_ERROR_CACHE_MISS_ID = -400;
NET_ERROR_CACHE_MISS_Name = 'NET_ERROR_CACHE_MISS';
NET_ERROR_CACHE_MISS_Des = 'The cache does not have the requested entry.';

NET_ERROR_CACHE_READ_FAILURE_ID = -401;
NET_ERROR_CACHE_READ_FAILURE_Name = 'NET_ERROR_CACHE_READ_FAILURE';
NET_ERROR_CACHE_READ_FAILURE_Des = 'Unable to read from the disk cache.';

NET_ERROR_CACHE_WRITE_FAILURE_ID = -402;
NET_ERROR_CACHE_WRITE_FAILURE_Name = 'NET_ERROR_CACHE_WRITE_FAILURE';
NET_ERROR_CACHE_WRITE_FAILURE_Des = 'Unable to write to the disk cache.';

NET_ERROR_CACHE_OPERATION_NOT_SUPPORTED_ID = -403;
NET_ERROR_CACHE_OPERATION_NOT_SUPPORTED_Name = 'NET_ERROR_CACHE_OPERATION_NOT_SUPPORTED';
NET_ERROR_CACHE_OPERATION_NOT_SUPPORTED_Des = 'The operation is not supported for this entry.';

NET_ERROR_CACHE_OPEN_FAILURE_ID = -404;
NET_ERROR_CACHE_OPEN_FAILURE_Name = 'NET_ERROR_CACHE_OPEN_FAILURE';
NET_ERROR_CACHE_OPEN_FAILURE_Des = 'The disk cache is unable to open this entry.';

NET_ERROR_CACHE_CREATE_FAILURE_ID = -405;
NET_ERROR_CACHE_CREATE_FAILURE_Name = 'NET_ERROR_CACHE_CREATE_FAILURE';
NET_ERROR_CACHE_CREATE_FAILURE_Des = 'The disk cache is unable to create this entry.';

NET_ERROR_CACHE_RACE_ID = -406;
NET_ERROR_CACHE_RACE_Name = 'NET_ERROR_CACHE_RACE';
NET_ERROR_CACHE_RACE_Des = 'Multiple transactions are racing to create disk cache entries. This is an' + #13 + 'internal error returned from the HttpCache to the HttpCacheTransaction that' + #13 + 'tells the transaction to restart the entry-creation logic because the state' + #13 + 'of the cache has changed.';

NET_ERROR_INSECURE_RESPONSE_ID = -501;
NET_ERROR_INSECURE_RESPONSE_Name = 'NET_ERROR_INSECURE_RESPONSE';
NET_ERROR_INSECURE_RESPONSE_Des = 'The server''s response was insecure (e.g. there was a cert error).';

NET_ERROR_NO_PRIVATE_KEY_FOR_CERT_ID = -502;
NET_ERROR_NO_PRIVATE_KEY_FOR_CERT_Name = 'NET_ERROR_NO_PRIVATE_KEY_FOR_CERT';
NET_ERROR_NO_PRIVATE_KEY_FOR_CERT_Des = 'The server responded to a <keygen> with a generated client cert that we' + #13 + 'don''t have the matching private key for.';

NET_ERROR_ADD_USER_CERT_FAILED_ID = -503;
NET_ERROR_ADD_USER_CERT_FAILED_Name = 'NET_ERROR_ADD_USER_CERT_FAILED';
NET_ERROR_ADD_USER_CERT_FAILED_Des = 'An error adding to the OS certificate database (e.g. OS X Keychain).';

NET_ERROR_FTP_FAILED_ID = -601;
NET_ERROR_FTP_FAILED_Name = 'NET_ERROR_FTP_FAILED';
NET_ERROR_FTP_FAILED_Des = '*** Code -600 is reserved (was FTP_PASV_COMMAND_FAILED). ***' + #13 + 'A generic error for failed FTP control connection command.' + #13 + 'If possible, please use or add a more specific error code.';

NET_ERROR_FTP_SERVICE_UNAVAILABLE_ID = -602;
NET_ERROR_FTP_SERVICE_UNAVAILABLE_Name = 'NET_ERROR_FTP_SERVICE_UNAVAILABLE';
NET_ERROR_FTP_SERVICE_UNAVAILABLE_Des = 'The server cannot fulfill the request at this point. This is a temporary' + #13 + 'error.' + #13 + 'FTP response code 421.';

NET_ERROR_FTP_TRANSFER_ABORTED_ID = -603;
NET_ERROR_FTP_TRANSFER_ABORTED_Name = 'NET_ERROR_FTP_TRANSFER_ABORTED';
NET_ERROR_FTP_TRANSFER_ABORTED_Des = 'The server has aborted the transfer.' + #13 + 'FTP response code 426.';

NET_ERROR_FTP_FILE_BUSY_ID = -604;
NET_ERROR_FTP_FILE_BUSY_Name = 'NET_ERROR_FTP_FILE_BUSY';
NET_ERROR_FTP_FILE_BUSY_Des = 'The file is busy, or some other temporary error condition on opening' + #13 + 'the file.' + #13 + 'FTP response code 450.';

NET_ERROR_FTP_SYNTAX_ERROR_ID = -605;
NET_ERROR_FTP_SYNTAX_ERROR_Name = 'NET_ERROR_FTP_SYNTAX_ERROR';
NET_ERROR_FTP_SYNTAX_ERROR_Des = 'Server rejected our command because of syntax errors.' + #13 + 'FTP response codes 500, 501.';

NET_ERROR_FTP_COMMAND_NOT_SUPPORTED_ID = -606;
NET_ERROR_FTP_COMMAND_NOT_SUPPORTED_Name = 'NET_ERROR_FTP_COMMAND_NOT_SUPPORTED';
NET_ERROR_FTP_COMMAND_NOT_SUPPORTED_Des = 'Server does not support the command we issued.' + #13 + 'FTP response codes 502, 504.';

NET_ERROR_FTP_BAD_COMMAND_SEQUENCE_ID = -607;
NET_ERROR_FTP_BAD_COMMAND_SEQUENCE_Name = 'NET_ERROR_FTP_BAD_COMMAND_SEQUENCE';
NET_ERROR_FTP_BAD_COMMAND_SEQUENCE_Des = 'Server rejected our command because we didn''t issue the commands in right' + #13 + 'order.' + #13 + 'FTP response code 503.';

NET_ERROR_PKCS12_IMPORT_BAD_PASSWORD_ID = -701;
NET_ERROR_PKCS12_IMPORT_BAD_PASSWORD_Name = 'NET_ERROR_PKCS12_IMPORT_BAD_PASSWORD';
NET_ERROR_PKCS12_IMPORT_BAD_PASSWORD_Des = 'PKCS #12 import failed due to incorrect password.';

NET_ERROR_PKCS12_IMPORT_FAILED_ID = -702;
NET_ERROR_PKCS12_IMPORT_FAILED_Name = 'NET_ERROR_PKCS12_IMPORT_FAILED';
NET_ERROR_PKCS12_IMPORT_FAILED_Des = 'PKCS #12 import failed due to other error.';

NET_ERROR_IMPORT_CA_CERT_NOT_CA_ID = -703;
NET_ERROR_IMPORT_CA_CERT_NOT_CA_Name = 'NET_ERROR_IMPORT_CA_CERT_NOT_CA';
NET_ERROR_IMPORT_CA_CERT_NOT_CA_Des = 'CA import failed - not a CA cert.';

NET_ERROR_IMPORT_CERT_ALREADY_EXISTS_ID = -704;
NET_ERROR_IMPORT_CERT_ALREADY_EXISTS_Name = 'NET_ERROR_IMPORT_CERT_ALREADY_EXISTS';
NET_ERROR_IMPORT_CERT_ALREADY_EXISTS_Des = 'Import failed - certificate already exists in database.' + #13 + 'Note it''s a little weird this is an error but reimporting a PKCS12 is ok' + #13 + '(no-op).  That''s how Mozilla does it, though.';

NET_ERROR_IMPORT_CA_CERT_FAILED_ID = -705;
NET_ERROR_IMPORT_CA_CERT_FAILED_Name = 'NET_ERROR_IMPORT_CA_CERT_FAILED';
NET_ERROR_IMPORT_CA_CERT_FAILED_Des = 'CA import failed due to some other error.';

NET_ERROR_IMPORT_SERVER_CERT_FAILED_ID = -706;
NET_ERROR_IMPORT_SERVER_CERT_FAILED_Name = 'NET_ERROR_IMPORT_SERVER_CERT_FAILED';
NET_ERROR_IMPORT_SERVER_CERT_FAILED_Des = 'Server certificate import failed due to some internal error.';

NET_ERROR_PKCS12_IMPORT_INVALID_MAC_ID = -707;
NET_ERROR_PKCS12_IMPORT_INVALID_MAC_Name = 'NET_ERROR_PKCS12_IMPORT_INVALID_MAC';
NET_ERROR_PKCS12_IMPORT_INVALID_MAC_Des = 'PKCS #12 import failed due to invalid MAC.';

NET_ERROR_PKCS12_IMPORT_INVALID_FILE_ID = -708;
NET_ERROR_PKCS12_IMPORT_INVALID_FILE_Name = 'NET_ERROR_PKCS12_IMPORT_INVALID_FILE';
NET_ERROR_PKCS12_IMPORT_INVALID_FILE_Des = 'PKCS #12 import failed due to invalid/corrupt file.';

NET_ERROR_PKCS12_IMPORT_UNSUPPORTED_ID = -709;
NET_ERROR_PKCS12_IMPORT_UNSUPPORTED_Name = 'NET_ERROR_PKCS12_IMPORT_UNSUPPORTED';
NET_ERROR_PKCS12_IMPORT_UNSUPPORTED_Des = 'PKCS #12 import failed due to unsupported features.';

NET_ERROR_KEY_GENERATION_FAILED_ID = -710;
NET_ERROR_KEY_GENERATION_FAILED_Name = 'NET_ERROR_KEY_GENERATION_FAILED';
NET_ERROR_KEY_GENERATION_FAILED_Des = 'Key generation failed.';

NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_FAILED_ID = -711;
NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_FAILED_Name = 'NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_FAILED';
NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_FAILED_Des = 'Server-bound certificate generation failed.';

NET_ERROR_PRIVATE_KEY_EXPORT_FAILED_ID = -712;
NET_ERROR_PRIVATE_KEY_EXPORT_FAILED_Name = 'NET_ERROR_PRIVATE_KEY_EXPORT_FAILED';
NET_ERROR_PRIVATE_KEY_EXPORT_FAILED_Des = 'Failure to export private key.';

NET_ERROR_DNS_MALFORMED_RESPONSE_ID = -800;
NET_ERROR_DNS_MALFORMED_RESPONSE_Name = 'NET_ERROR_DNS_MALFORMED_RESPONSE';
NET_ERROR_DNS_MALFORMED_RESPONSE_Des = 'DNS error codes.' + #13 + 'DNS resolver received a malformed response.';

NET_ERROR_DNS_SERVER_REQUIRES_TCP_ID = -801;
NET_ERROR_DNS_SERVER_REQUIRES_TCP_Name = 'NET_ERROR_DNS_SERVER_REQUIRES_TCP';
NET_ERROR_DNS_SERVER_REQUIRES_TCP_Des = 'DNS server requires TCP';

NET_ERROR_DNS_SERVER_FAILED_ID = -802;
NET_ERROR_DNS_SERVER_FAILED_Name = 'NET_ERROR_DNS_SERVER_FAILED';
NET_ERROR_DNS_SERVER_FAILED_Des = 'DNS server failed.  This error is returned for all of the following' + #13 + 'error conditions:' + #13 + '1 - Format error - The name server was unable to interpret the query.' + #13 + '2 - Server failure - The name server was unable to process this query' + #13 + '    due to a problem with the name server.' + #13 + '4 - Not Implemented - The name server does not support the requested' + #13 + '    kind of query.' + #13 + '5 - Refused - The name server refuses to perform the specified' + #13 + '    operation for policy reasons.';

NET_ERROR_DNS_TIMED_OUT_ID = -803;
NET_ERROR_DNS_TIMED_OUT_Name = 'NET_ERROR_DNS_TIMED_OUT';
NET_ERROR_DNS_TIMED_OUT_Des = 'DNS transaction timed out.';

NET_ERROR_DNS_CACHE_MISS_ID = -804;
NET_ERROR_DNS_CACHE_MISS_Name = 'NET_ERROR_DNS_CACHE_MISS';
NET_ERROR_DNS_CACHE_MISS_Des = 'The entry was not found in cache, for cache-only lookups.';

NET_ERROR_DNS_SEARCH_EMPTY_ID = -805;
NET_ERROR_DNS_SEARCH_EMPTY_Name = 'NET_ERROR_DNS_SEARCH_EMPTY';
NET_ERROR_DNS_SEARCH_EMPTY_Des = 'Suffix search list rules prevent resolution of the given host name.';

NET_ERROR_DNS_SORT_ERROR_ID = -806;
NET_ERROR_DNS_SORT_ERROR_Name = 'NET_ERROR_DNS_SORT_ERROR';
NET_ERROR_DNS_SORT_ERROR_Des = 'Failed to sort addresses according to RFC3484.';







 

var
  CefErrorManager: TCefErrorManager;

implementation

{ CefErrorManager }

constructor TCefErrorManager.Create;
begin

end;

destructor TCefErrorManager.Destroy;
begin
  FErrorData.Free;
  inherited;
end;

function TCefErrorManager.GetErrorDescription(const aErrCode: integer;
  var aErrorName, aErrorDescription: string): Boolean;
var
  errData: TStrings;
begin
  InitData;
  Result := FErrorData.TryGetValue(aErrCode, errData);
  if Result and Assigned(errData) then
  begin
    aErrorName := errData[0];
    aErrorDescription := errData[1];
  end; 
end;

procedure TCefErrorManager.InitData;

  procedure AddErrorData(aErrCode: integer;
    aErrorName, aErrorDescription: string);
  var
    aDes: TStrings;
  begin
    aDes := TStringList.Create;
    try
      aDes.Add(aErrorName);
      aDes.Add(aErrorDescription);
      FErrorData.Add(aErrCode, aDes);
    finally
      //aDes.Free;
    end;
  end;

begin
  if Not Assigned(FErrorData) then
  begin
    FErrorData := TObjectDictionary<integer, TStrings>.Create([doOwnsValues]); 
    
    AddErrorData(NET_ERROR_IO_PENDING_ID, NET_ERROR_IO_PENDING_Name, NET_ERROR_IO_PENDING_Des);
    AddErrorData(NET_ERROR_FAILED_ID, NET_ERROR_FAILED_Name, NET_ERROR_FAILED_Des);
    AddErrorData(NET_ERROR_ABORTED_ID, NET_ERROR_ABORTED_Name, NET_ERROR_ABORTED_Des);
    AddErrorData(NET_ERROR_INVALID_ARGUMENT_ID, NET_ERROR_INVALID_ARGUMENT_Name, NET_ERROR_INVALID_ARGUMENT_Des);
    AddErrorData(NET_ERROR_INVALID_HANDLE_ID, NET_ERROR_INVALID_HANDLE_Name, NET_ERROR_INVALID_HANDLE_Des);
    AddErrorData(NET_ERROR_FILE_NOT_FOUND_ID, NET_ERROR_FILE_NOT_FOUND_Name, NET_ERROR_FILE_NOT_FOUND_Des);
    AddErrorData(NET_ERROR_TIMED_OUT_ID, NET_ERROR_TIMED_OUT_Name, NET_ERROR_TIMED_OUT_Des);
    AddErrorData(NET_ERROR_FILE_TOO_BIG_ID, NET_ERROR_FILE_TOO_BIG_Name, NET_ERROR_FILE_TOO_BIG_Des);
    AddErrorData(NET_ERROR_UNEXPECTED_ID, NET_ERROR_UNEXPECTED_Name, NET_ERROR_UNEXPECTED_Des);
    AddErrorData(NET_ERROR_ACCESS_DENIED_ID, NET_ERROR_ACCESS_DENIED_Name, NET_ERROR_ACCESS_DENIED_Des);
    AddErrorData(NET_ERROR_NOT_IMPLEMENTED_ID, NET_ERROR_NOT_IMPLEMENTED_Name, NET_ERROR_NOT_IMPLEMENTED_Des);
    AddErrorData(NET_ERROR_INSUFFICIENT_RESOURCES_ID, NET_ERROR_INSUFFICIENT_RESOURCES_Name, NET_ERROR_INSUFFICIENT_RESOURCES_Des);
    AddErrorData(NET_ERROR_OUT_OF_MEMORY_ID, NET_ERROR_OUT_OF_MEMORY_Name, NET_ERROR_OUT_OF_MEMORY_Des);
    AddErrorData(NET_ERROR_UPLOAD_FILE_CHANGED_ID, NET_ERROR_UPLOAD_FILE_CHANGED_Name, NET_ERROR_UPLOAD_FILE_CHANGED_Des);
    AddErrorData(NET_ERROR_SOCKET_NOT_CONNECTED_ID, NET_ERROR_SOCKET_NOT_CONNECTED_Name, NET_ERROR_SOCKET_NOT_CONNECTED_Des);
    AddErrorData(NET_ERROR_FILE_EXISTS_ID, NET_ERROR_FILE_EXISTS_Name, NET_ERROR_FILE_EXISTS_Des);
    AddErrorData(NET_ERROR_FILE_PATH_TOO_LONG_ID, NET_ERROR_FILE_PATH_TOO_LONG_Name, NET_ERROR_FILE_PATH_TOO_LONG_Des);
    AddErrorData(NET_ERROR_FILE_NO_SPACE_ID, NET_ERROR_FILE_NO_SPACE_Name, NET_ERROR_FILE_NO_SPACE_Des);
    AddErrorData(NET_ERROR_FILE_VIRUS_INFECTED_ID, NET_ERROR_FILE_VIRUS_INFECTED_Name, NET_ERROR_FILE_VIRUS_INFECTED_Des);
    AddErrorData(NET_ERROR_BLOCKED_BY_CLIENT_ID, NET_ERROR_BLOCKED_BY_CLIENT_Name, NET_ERROR_BLOCKED_BY_CLIENT_Des);
    AddErrorData(NET_ERROR_NETWORK_CHANGED_ID, NET_ERROR_NETWORK_CHANGED_Name, NET_ERROR_NETWORK_CHANGED_Des);
    AddErrorData(NET_ERROR_BLOCKED_BY_ADMINISTRATOR_ID, NET_ERROR_BLOCKED_BY_ADMINISTRATOR_Name, NET_ERROR_BLOCKED_BY_ADMINISTRATOR_Des);
    AddErrorData(NET_ERROR_CONNECTION_CLOSED_ID, NET_ERROR_CONNECTION_CLOSED_Name, NET_ERROR_CONNECTION_CLOSED_Des);
    AddErrorData(NET_ERROR_CONNECTION_RESET_ID, NET_ERROR_CONNECTION_RESET_Name, NET_ERROR_CONNECTION_RESET_Des);
    AddErrorData(NET_ERROR_CONNECTION_REFUSED_ID, NET_ERROR_CONNECTION_REFUSED_Name, NET_ERROR_CONNECTION_REFUSED_Des);
    AddErrorData(NET_ERROR_CONNECTION_ABORTED_ID, NET_ERROR_CONNECTION_ABORTED_Name, NET_ERROR_CONNECTION_ABORTED_Des);
    AddErrorData(NET_ERROR_CONNECTION_FAILED_ID, NET_ERROR_CONNECTION_FAILED_Name, NET_ERROR_CONNECTION_FAILED_Des);
    AddErrorData(NET_ERROR_NAME_NOT_RESOLVED_ID, NET_ERROR_NAME_NOT_RESOLVED_Name, NET_ERROR_NAME_NOT_RESOLVED_Des);
    AddErrorData(NET_ERROR_INTERNET_DISCONNECTED_ID, NET_ERROR_INTERNET_DISCONNECTED_Name, NET_ERROR_INTERNET_DISCONNECTED_Des);
    AddErrorData(NET_ERROR_SSL_PROTOCOL_ERROR_ID, NET_ERROR_SSL_PROTOCOL_ERROR_Name, NET_ERROR_SSL_PROTOCOL_ERROR_Des);
    AddErrorData(NET_ERROR_ADDRESS_INVALID_ID, NET_ERROR_ADDRESS_INVALID_Name, NET_ERROR_ADDRESS_INVALID_Des);
    AddErrorData(NET_ERROR_ADDRESS_UNREACHABLE_ID, NET_ERROR_ADDRESS_UNREACHABLE_Name, NET_ERROR_ADDRESS_UNREACHABLE_Des);
    AddErrorData(NET_ERROR_SSL_CLIENT_AUTH_CERT_NEEDED_ID, NET_ERROR_SSL_CLIENT_AUTH_CERT_NEEDED_Name, NET_ERROR_SSL_CLIENT_AUTH_CERT_NEEDED_Des);
    AddErrorData(NET_ERROR_TUNNEL_CONNECTION_FAILED_ID, NET_ERROR_TUNNEL_CONNECTION_FAILED_Name, NET_ERROR_TUNNEL_CONNECTION_FAILED_Des);
    AddErrorData(NET_ERROR_NO_SSL_VERSIONS_ENABLED_ID, NET_ERROR_NO_SSL_VERSIONS_ENABLED_Name, NET_ERROR_NO_SSL_VERSIONS_ENABLED_Des);
    AddErrorData(NET_ERROR_SSL_VERSION_OR_CIPHER_MISMATCH_ID, NET_ERROR_SSL_VERSION_OR_CIPHER_MISMATCH_Name, NET_ERROR_SSL_VERSION_OR_CIPHER_MISMATCH_Des);
    AddErrorData(NET_ERROR_SSL_RENEGOTIATION_REQUESTED_ID, NET_ERROR_SSL_RENEGOTIATION_REQUESTED_Name, NET_ERROR_SSL_RENEGOTIATION_REQUESTED_Des);
    AddErrorData(NET_ERROR_PROXY_AUTH_UNSUPPORTED_ID, NET_ERROR_PROXY_AUTH_UNSUPPORTED_Name, NET_ERROR_PROXY_AUTH_UNSUPPORTED_Des);
    AddErrorData(NET_ERROR_CERT_ERROR_IN_SSL_RENEGOTIATION_ID, NET_ERROR_CERT_ERROR_IN_SSL_RENEGOTIATION_Name, NET_ERROR_CERT_ERROR_IN_SSL_RENEGOTIATION_Des);
    AddErrorData(NET_ERROR_BAD_SSL_CLIENT_AUTH_CERT_ID, NET_ERROR_BAD_SSL_CLIENT_AUTH_CERT_Name, NET_ERROR_BAD_SSL_CLIENT_AUTH_CERT_Des);
    AddErrorData(NET_ERROR_CONNECTION_TIMED_OUT_ID, NET_ERROR_CONNECTION_TIMED_OUT_Name, NET_ERROR_CONNECTION_TIMED_OUT_Des);
    AddErrorData(NET_ERROR_HOST_RESOLVER_QUEUE_TOO_LARGE_ID, NET_ERROR_HOST_RESOLVER_QUEUE_TOO_LARGE_Name, NET_ERROR_HOST_RESOLVER_QUEUE_TOO_LARGE_Des);
    AddErrorData(NET_ERROR_SOCKS_CONNECTION_FAILED_ID, NET_ERROR_SOCKS_CONNECTION_FAILED_Name, NET_ERROR_SOCKS_CONNECTION_FAILED_Des);
    AddErrorData(NET_ERROR_SOCKS_CONNECTION_HOST_UNREACHABLE_ID, NET_ERROR_SOCKS_CONNECTION_HOST_UNREACHABLE_Name, NET_ERROR_SOCKS_CONNECTION_HOST_UNREACHABLE_Des);
    AddErrorData(NET_ERROR_NPN_NEGOTIATION_FAILED_ID, NET_ERROR_NPN_NEGOTIATION_FAILED_Name, NET_ERROR_NPN_NEGOTIATION_FAILED_Des);
    AddErrorData(NET_ERROR_SSL_NO_RENEGOTIATION_ID, NET_ERROR_SSL_NO_RENEGOTIATION_Name, NET_ERROR_SSL_NO_RENEGOTIATION_Des);
    AddErrorData(NET_ERROR_WINSOCK_UNEXPECTED_WRITTEN_BYTES_ID, NET_ERROR_WINSOCK_UNEXPECTED_WRITTEN_BYTES_Name, NET_ERROR_WINSOCK_UNEXPECTED_WRITTEN_BYTES_Des);
    AddErrorData(NET_ERROR_SSL_DECOMPRESSION_FAILURE_ALERT_ID, NET_ERROR_SSL_DECOMPRESSION_FAILURE_ALERT_Name, NET_ERROR_SSL_DECOMPRESSION_FAILURE_ALERT_Des);
    AddErrorData(NET_ERROR_SSL_BAD_RECORD_MAC_ALERT_ID, NET_ERROR_SSL_BAD_RECORD_MAC_ALERT_Name, NET_ERROR_SSL_BAD_RECORD_MAC_ALERT_Des);
    AddErrorData(NET_ERROR_PROXY_AUTH_REQUESTED_ID, NET_ERROR_PROXY_AUTH_REQUESTED_Name, NET_ERROR_PROXY_AUTH_REQUESTED_Des);
    AddErrorData(NET_ERROR_SSL_UNSAFE_NEGOTIATION_ID, NET_ERROR_SSL_UNSAFE_NEGOTIATION_Name, NET_ERROR_SSL_UNSAFE_NEGOTIATION_Des);
    AddErrorData(NET_ERROR_SSL_WEAK_SERVER_EPHEMERAL_DH_KEY_ID, NET_ERROR_SSL_WEAK_SERVER_EPHEMERAL_DH_KEY_Name, NET_ERROR_SSL_WEAK_SERVER_EPHEMERAL_DH_KEY_Des);
    AddErrorData(NET_ERROR_PROXY_CONNECTION_FAILED_ID, NET_ERROR_PROXY_CONNECTION_FAILED_Name, NET_ERROR_PROXY_CONNECTION_FAILED_Des);
    AddErrorData(NET_ERROR_MANDATORY_PROXY_CONFIGURATION_FAILED_ID, NET_ERROR_MANDATORY_PROXY_CONFIGURATION_FAILED_Name, NET_ERROR_MANDATORY_PROXY_CONFIGURATION_FAILED_Des);
    AddErrorData(NET_ERROR_PRECONNECT_MAX_SOCKET_LIMIT_ID, NET_ERROR_PRECONNECT_MAX_SOCKET_LIMIT_Name, NET_ERROR_PRECONNECT_MAX_SOCKET_LIMIT_Des);
    AddErrorData(NET_ERROR_SSL_CLIENT_AUTH_PRIVATE_KEY_ACCESS_DENIED_ID, NET_ERROR_SSL_CLIENT_AUTH_PRIVATE_KEY_ACCESS_DENIED_Name, NET_ERROR_SSL_CLIENT_AUTH_PRIVATE_KEY_ACCESS_DENIED_Des);
    AddErrorData(NET_ERROR_SSL_CLIENT_AUTH_CERT_NO_PRIVATE_KEY_ID, NET_ERROR_SSL_CLIENT_AUTH_CERT_NO_PRIVATE_KEY_Name, NET_ERROR_SSL_CLIENT_AUTH_CERT_NO_PRIVATE_KEY_Des);
    AddErrorData(NET_ERROR_PROXY_CERTIFICATE_INVALID_ID, NET_ERROR_PROXY_CERTIFICATE_INVALID_Name, NET_ERROR_PROXY_CERTIFICATE_INVALID_Des);
    AddErrorData(NET_ERROR_NAME_RESOLUTION_FAILED_ID, NET_ERROR_NAME_RESOLUTION_FAILED_Name, NET_ERROR_NAME_RESOLUTION_FAILED_Des);
    AddErrorData(NET_ERROR_NETWORK_ACCESS_DENIED_ID, NET_ERROR_NETWORK_ACCESS_DENIED_Name, NET_ERROR_NETWORK_ACCESS_DENIED_Des);
    AddErrorData(NET_ERROR_TEMPORARILY_THROTTLED_ID, NET_ERROR_TEMPORARILY_THROTTLED_Name, NET_ERROR_TEMPORARILY_THROTTLED_Des);
    AddErrorData(NET_ERROR_HTTPS_PROXY_TUNNEL_RESPONSE_ID, NET_ERROR_HTTPS_PROXY_TUNNEL_RESPONSE_Name, NET_ERROR_HTTPS_PROXY_TUNNEL_RESPONSE_Des);
    AddErrorData(NET_ERROR_SSL_CLIENT_AUTH_SIGNATURE_FAILED_ID, NET_ERROR_SSL_CLIENT_AUTH_SIGNATURE_FAILED_Name, NET_ERROR_SSL_CLIENT_AUTH_SIGNATURE_FAILED_Des);
    AddErrorData(NET_ERROR_MSG_TOO_BIG_ID, NET_ERROR_MSG_TOO_BIG_Name, NET_ERROR_MSG_TOO_BIG_Des);
    AddErrorData(NET_ERROR_SPDY_SESSION_ALREADY_EXISTS_ID, NET_ERROR_SPDY_SESSION_ALREADY_EXISTS_Name, NET_ERROR_SPDY_SESSION_ALREADY_EXISTS_Des);
    AddErrorData(NET_ERROR_WS_PROTOCOL_ERROR_ID, NET_ERROR_WS_PROTOCOL_ERROR_Name, NET_ERROR_WS_PROTOCOL_ERROR_Des);
    AddErrorData(NET_ERROR_PROTOCOL_SWITCHED_ID, NET_ERROR_PROTOCOL_SWITCHED_Name, NET_ERROR_PROTOCOL_SWITCHED_Des);
    AddErrorData(NET_ERROR_ADDRESS_IN_USE_ID, NET_ERROR_ADDRESS_IN_USE_Name, NET_ERROR_ADDRESS_IN_USE_Des);
    AddErrorData(NET_ERROR_SSL_HANDSHAKE_NOT_COMPLETED_ID, NET_ERROR_SSL_HANDSHAKE_NOT_COMPLETED_Name, NET_ERROR_SSL_HANDSHAKE_NOT_COMPLETED_Des);
    AddErrorData(NET_ERROR_SSL_BAD_PEER_PUBLIC_KEY_ID, NET_ERROR_SSL_BAD_PEER_PUBLIC_KEY_Name, NET_ERROR_SSL_BAD_PEER_PUBLIC_KEY_Des);
    AddErrorData(NET_ERROR_SSL_PINNED_KEY_NOT_IN_CERT_CHAIN_ID, NET_ERROR_SSL_PINNED_KEY_NOT_IN_CERT_CHAIN_Name, NET_ERROR_SSL_PINNED_KEY_NOT_IN_CERT_CHAIN_Des);
    AddErrorData(NET_ERROR_CLIENT_AUTH_CERT_TYPE_UNSUPPORTED_ID, NET_ERROR_CLIENT_AUTH_CERT_TYPE_UNSUPPORTED_Name, NET_ERROR_CLIENT_AUTH_CERT_TYPE_UNSUPPORTED_Des);
    AddErrorData(NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_TYPE_MISMATCH_ID, NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_TYPE_MISMATCH_Name, NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_TYPE_MISMATCH_Des);
    AddErrorData(NET_ERROR_CERT_COMMON_NAME_INVALID_ID, NET_ERROR_CERT_COMMON_NAME_INVALID_Name, NET_ERROR_CERT_COMMON_NAME_INVALID_Des);
    AddErrorData(NET_ERROR_CERT_DATE_INVALID_ID, NET_ERROR_CERT_DATE_INVALID_Name, NET_ERROR_CERT_DATE_INVALID_Des);
    AddErrorData(NET_ERROR_CERT_AUTHORITY_INVALID_ID, NET_ERROR_CERT_AUTHORITY_INVALID_Name, NET_ERROR_CERT_AUTHORITY_INVALID_Des);
    AddErrorData(NET_ERROR_CERT_CONTAINS_ERRORS_ID, NET_ERROR_CERT_CONTAINS_ERRORS_Name, NET_ERROR_CERT_CONTAINS_ERRORS_Des);
    AddErrorData(NET_ERROR_CERT_NO_REVOCATION_MECHANISM_ID, NET_ERROR_CERT_NO_REVOCATION_MECHANISM_Name, NET_ERROR_CERT_NO_REVOCATION_MECHANISM_Des);
    AddErrorData(NET_ERROR_CERT_UNABLE_TO_CHECK_REVOCATION_ID, NET_ERROR_CERT_UNABLE_TO_CHECK_REVOCATION_Name, NET_ERROR_CERT_UNABLE_TO_CHECK_REVOCATION_Des);
    AddErrorData(NET_ERROR_CERT_REVOKED_ID, NET_ERROR_CERT_REVOKED_Name, NET_ERROR_CERT_REVOKED_Des);
    AddErrorData(NET_ERROR_CERT_INVALID_ID, NET_ERROR_CERT_INVALID_Name, NET_ERROR_CERT_INVALID_Des);
    AddErrorData(NET_ERROR_CERT_WEAK_SIGNATURE_ALGORITHM_ID, NET_ERROR_CERT_WEAK_SIGNATURE_ALGORITHM_Name, NET_ERROR_CERT_WEAK_SIGNATURE_ALGORITHM_Des);
    AddErrorData(NET_ERROR_CERT_NON_UNIQUE_NAME_ID, NET_ERROR_CERT_NON_UNIQUE_NAME_Name, NET_ERROR_CERT_NON_UNIQUE_NAME_Des);
    AddErrorData(NET_ERROR_CERT_WEAK_KEY_ID, NET_ERROR_CERT_WEAK_KEY_Name, NET_ERROR_CERT_WEAK_KEY_Des);
    AddErrorData(NET_ERROR_CERT_END_ID, NET_ERROR_CERT_END_Name, NET_ERROR_CERT_END_Des);
    AddErrorData(NET_ERROR_INVALID_URL_ID, NET_ERROR_INVALID_URL_Name, NET_ERROR_INVALID_URL_Des);
    AddErrorData(NET_ERROR_DISALLOWED_URL_SCHEME_ID, NET_ERROR_DISALLOWED_URL_SCHEME_Name, NET_ERROR_DISALLOWED_URL_SCHEME_Des);
    AddErrorData(NET_ERROR_UNKNOWN_URL_SCHEME_ID, NET_ERROR_UNKNOWN_URL_SCHEME_Name, NET_ERROR_UNKNOWN_URL_SCHEME_Des);
    AddErrorData(NET_ERROR_TOO_MANY_REDIRECTS_ID, NET_ERROR_TOO_MANY_REDIRECTS_Name, NET_ERROR_TOO_MANY_REDIRECTS_Des);
    AddErrorData(NET_ERROR_UNSAFE_REDIRECT_ID, NET_ERROR_UNSAFE_REDIRECT_Name, NET_ERROR_UNSAFE_REDIRECT_Des);
    AddErrorData(NET_ERROR_UNSAFE_PORT_ID, NET_ERROR_UNSAFE_PORT_Name, NET_ERROR_UNSAFE_PORT_Des);
    AddErrorData(NET_ERROR_INVALID_RESPONSE_ID, NET_ERROR_INVALID_RESPONSE_Name, NET_ERROR_INVALID_RESPONSE_Des);
    AddErrorData(NET_ERROR_INVALID_CHUNKED_ENCODING_ID, NET_ERROR_INVALID_CHUNKED_ENCODING_Name, NET_ERROR_INVALID_CHUNKED_ENCODING_Des);
    AddErrorData(NET_ERROR_METHOD_NOT_SUPPORTED_ID, NET_ERROR_METHOD_NOT_SUPPORTED_Name, NET_ERROR_METHOD_NOT_SUPPORTED_Des);
    AddErrorData(NET_ERROR_UNEXPECTED_PROXY_AUTH_ID, NET_ERROR_UNEXPECTED_PROXY_AUTH_Name, NET_ERROR_UNEXPECTED_PROXY_AUTH_Des);
    AddErrorData(NET_ERROR_EMPTY_RESPONSE_ID, NET_ERROR_EMPTY_RESPONSE_Name, NET_ERROR_EMPTY_RESPONSE_Des);
    AddErrorData(NET_ERROR_RESPONSE_HEADERS_TOO_BIG_ID, NET_ERROR_RESPONSE_HEADERS_TOO_BIG_Name, NET_ERROR_RESPONSE_HEADERS_TOO_BIG_Des);
    AddErrorData(NET_ERROR_PAC_STATUS_NOT_OK_ID, NET_ERROR_PAC_STATUS_NOT_OK_Name, NET_ERROR_PAC_STATUS_NOT_OK_Des);
    AddErrorData(NET_ERROR_PAC_SCRIPT_FAILED_ID, NET_ERROR_PAC_SCRIPT_FAILED_Name, NET_ERROR_PAC_SCRIPT_FAILED_Des);
    AddErrorData(NET_ERROR_REQUEST_RANGE_NOT_SATISFIABLE_ID, NET_ERROR_REQUEST_RANGE_NOT_SATISFIABLE_Name, NET_ERROR_REQUEST_RANGE_NOT_SATISFIABLE_Des);
    AddErrorData(NET_ERROR_MALFORMED_IDENTITY_ID, NET_ERROR_MALFORMED_IDENTITY_Name, NET_ERROR_MALFORMED_IDENTITY_Des);
    AddErrorData(NET_ERROR_CONTENT_DECODING_FAILED_ID, NET_ERROR_CONTENT_DECODING_FAILED_Name, NET_ERROR_CONTENT_DECODING_FAILED_Des);
    AddErrorData(NET_ERROR_NETWORK_IO_SUSPENDED_ID, NET_ERROR_NETWORK_IO_SUSPENDED_Name, NET_ERROR_NETWORK_IO_SUSPENDED_Des);
    AddErrorData(NET_ERROR_SYN_REPLY_NOT_RECEIVED_ID, NET_ERROR_SYN_REPLY_NOT_RECEIVED_Name, NET_ERROR_SYN_REPLY_NOT_RECEIVED_Des);
    AddErrorData(NET_ERROR_ENCODING_CONVERSION_FAILED_ID, NET_ERROR_ENCODING_CONVERSION_FAILED_Name, NET_ERROR_ENCODING_CONVERSION_FAILED_Des);
    AddErrorData(NET_ERROR_UNRECOGNIZED_FTP_DIRECTORY_LISTING_FORMAT_ID, NET_ERROR_UNRECOGNIZED_FTP_DIRECTORY_LISTING_FORMAT_Name, NET_ERROR_UNRECOGNIZED_FTP_DIRECTORY_LISTING_FORMAT_Des);
    AddErrorData(NET_ERROR_INVALID_SPDY_STREAM_ID, NET_ERROR_INVALID_SPDY_STREAM_Name, NET_ERROR_INVALID_SPDY_STREAM_Des);
    AddErrorData(NET_ERROR_NO_SUPPORTED_PROXIES_ID, NET_ERROR_NO_SUPPORTED_PROXIES_Name, NET_ERROR_NO_SUPPORTED_PROXIES_Des);
    AddErrorData(NET_ERROR_SPDY_PROTOCOL_ERROR_ID, NET_ERROR_SPDY_PROTOCOL_ERROR_Name, NET_ERROR_SPDY_PROTOCOL_ERROR_Des);
    AddErrorData(NET_ERROR_INVALID_AUTH_CREDENTIALS_ID, NET_ERROR_INVALID_AUTH_CREDENTIALS_Name, NET_ERROR_INVALID_AUTH_CREDENTIALS_Des);
    AddErrorData(NET_ERROR_UNSUPPORTED_AUTH_SCHEME_ID, NET_ERROR_UNSUPPORTED_AUTH_SCHEME_Name, NET_ERROR_UNSUPPORTED_AUTH_SCHEME_Des);
    AddErrorData(NET_ERROR_ENCODING_DETECTION_FAILED_ID, NET_ERROR_ENCODING_DETECTION_FAILED_Name, NET_ERROR_ENCODING_DETECTION_FAILED_Des);
    AddErrorData(NET_ERROR_MISSING_AUTH_CREDENTIALS_ID, NET_ERROR_MISSING_AUTH_CREDENTIALS_Name, NET_ERROR_MISSING_AUTH_CREDENTIALS_Des);
    AddErrorData(NET_ERROR_UNEXPECTED_SECURITY_LIBRARY_STATUS_ID, NET_ERROR_UNEXPECTED_SECURITY_LIBRARY_STATUS_Name, NET_ERROR_UNEXPECTED_SECURITY_LIBRARY_STATUS_Des);
    AddErrorData(NET_ERROR_MISCONFIGURED_AUTH_ENVIRONMENT_ID, NET_ERROR_MISCONFIGURED_AUTH_ENVIRONMENT_Name, NET_ERROR_MISCONFIGURED_AUTH_ENVIRONMENT_Des);
    AddErrorData(NET_ERROR_UNDOCUMENTED_SECURITY_LIBRARY_STATUS_ID, NET_ERROR_UNDOCUMENTED_SECURITY_LIBRARY_STATUS_Name, NET_ERROR_UNDOCUMENTED_SECURITY_LIBRARY_STATUS_Des);
    AddErrorData(NET_ERROR_RESPONSE_BODY_TOO_BIG_TO_DRAIN_ID, NET_ERROR_RESPONSE_BODY_TOO_BIG_TO_DRAIN_Name, NET_ERROR_RESPONSE_BODY_TOO_BIG_TO_DRAIN_Des);
    AddErrorData(NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_LENGTH_ID, NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_LENGTH_Name, NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_LENGTH_Des);
    AddErrorData(NET_ERROR_INCOMPLETE_SPDY_HEADERS_ID, NET_ERROR_INCOMPLETE_SPDY_HEADERS_Name, NET_ERROR_INCOMPLETE_SPDY_HEADERS_Des);
    AddErrorData(NET_ERROR_PAC_NOT_IN_DHCP_ID, NET_ERROR_PAC_NOT_IN_DHCP_Name, NET_ERROR_PAC_NOT_IN_DHCP_Des);
    AddErrorData(NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_DISPOSITION_ID, NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_DISPOSITION_Name, NET_ERROR_RESPONSE_HEADERS_MULTIPLE_CONTENT_DISPOSITION_Des);
    AddErrorData(NET_ERROR_RESPONSE_HEADERS_MULTIPLE_LOCATION_ID, NET_ERROR_RESPONSE_HEADERS_MULTIPLE_LOCATION_Name, NET_ERROR_RESPONSE_HEADERS_MULTIPLE_LOCATION_Des);
    AddErrorData(NET_ERROR_SPDY_SERVER_REFUSED_STREAM_ID, NET_ERROR_SPDY_SERVER_REFUSED_STREAM_Name, NET_ERROR_SPDY_SERVER_REFUSED_STREAM_Des);
    AddErrorData(NET_ERROR_SPDY_PING_FAILED_ID, NET_ERROR_SPDY_PING_FAILED_Name, NET_ERROR_SPDY_PING_FAILED_Des);
    AddErrorData(NET_ERROR_PIPELINE_EVICTION_ID, NET_ERROR_PIPELINE_EVICTION_Name, NET_ERROR_PIPELINE_EVICTION_Des);
    AddErrorData(NET_ERROR_CONTENT_LENGTH_MISMATCH_ID, NET_ERROR_CONTENT_LENGTH_MISMATCH_Name, NET_ERROR_CONTENT_LENGTH_MISMATCH_Des);
    AddErrorData(NET_ERROR_INCOMPLETE_CHUNKED_ENCODING_ID, NET_ERROR_INCOMPLETE_CHUNKED_ENCODING_Name, NET_ERROR_INCOMPLETE_CHUNKED_ENCODING_Des);
    AddErrorData(NET_ERROR_QUIC_PROTOCOL_ERROR_ID, NET_ERROR_QUIC_PROTOCOL_ERROR_Name, NET_ERROR_QUIC_PROTOCOL_ERROR_Des);
    AddErrorData(NET_ERROR_CACHE_MISS_ID, NET_ERROR_CACHE_MISS_Name, NET_ERROR_CACHE_MISS_Des);
    AddErrorData(NET_ERROR_CACHE_READ_FAILURE_ID, NET_ERROR_CACHE_READ_FAILURE_Name, NET_ERROR_CACHE_READ_FAILURE_Des);
    AddErrorData(NET_ERROR_CACHE_WRITE_FAILURE_ID, NET_ERROR_CACHE_WRITE_FAILURE_Name, NET_ERROR_CACHE_WRITE_FAILURE_Des);
    AddErrorData(NET_ERROR_CACHE_OPERATION_NOT_SUPPORTED_ID, NET_ERROR_CACHE_OPERATION_NOT_SUPPORTED_Name, NET_ERROR_CACHE_OPERATION_NOT_SUPPORTED_Des);
    AddErrorData(NET_ERROR_CACHE_OPEN_FAILURE_ID, NET_ERROR_CACHE_OPEN_FAILURE_Name, NET_ERROR_CACHE_OPEN_FAILURE_Des);
    AddErrorData(NET_ERROR_CACHE_CREATE_FAILURE_ID, NET_ERROR_CACHE_CREATE_FAILURE_Name, NET_ERROR_CACHE_CREATE_FAILURE_Des);
    AddErrorData(NET_ERROR_CACHE_RACE_ID, NET_ERROR_CACHE_RACE_Name, NET_ERROR_CACHE_RACE_Des);
    AddErrorData(NET_ERROR_INSECURE_RESPONSE_ID, NET_ERROR_INSECURE_RESPONSE_Name, NET_ERROR_INSECURE_RESPONSE_Des);
    AddErrorData(NET_ERROR_NO_PRIVATE_KEY_FOR_CERT_ID, NET_ERROR_NO_PRIVATE_KEY_FOR_CERT_Name, NET_ERROR_NO_PRIVATE_KEY_FOR_CERT_Des);
    AddErrorData(NET_ERROR_ADD_USER_CERT_FAILED_ID, NET_ERROR_ADD_USER_CERT_FAILED_Name, NET_ERROR_ADD_USER_CERT_FAILED_Des);
    AddErrorData(NET_ERROR_FTP_FAILED_ID, NET_ERROR_FTP_FAILED_Name, NET_ERROR_FTP_FAILED_Des);
    AddErrorData(NET_ERROR_FTP_SERVICE_UNAVAILABLE_ID, NET_ERROR_FTP_SERVICE_UNAVAILABLE_Name, NET_ERROR_FTP_SERVICE_UNAVAILABLE_Des);
    AddErrorData(NET_ERROR_FTP_TRANSFER_ABORTED_ID, NET_ERROR_FTP_TRANSFER_ABORTED_Name, NET_ERROR_FTP_TRANSFER_ABORTED_Des);
    AddErrorData(NET_ERROR_FTP_FILE_BUSY_ID, NET_ERROR_FTP_FILE_BUSY_Name, NET_ERROR_FTP_FILE_BUSY_Des);
    AddErrorData(NET_ERROR_FTP_SYNTAX_ERROR_ID, NET_ERROR_FTP_SYNTAX_ERROR_Name, NET_ERROR_FTP_SYNTAX_ERROR_Des);
    AddErrorData(NET_ERROR_FTP_COMMAND_NOT_SUPPORTED_ID, NET_ERROR_FTP_COMMAND_NOT_SUPPORTED_Name, NET_ERROR_FTP_COMMAND_NOT_SUPPORTED_Des);
    AddErrorData(NET_ERROR_FTP_BAD_COMMAND_SEQUENCE_ID, NET_ERROR_FTP_BAD_COMMAND_SEQUENCE_Name, NET_ERROR_FTP_BAD_COMMAND_SEQUENCE_Des);
    AddErrorData(NET_ERROR_PKCS12_IMPORT_BAD_PASSWORD_ID, NET_ERROR_PKCS12_IMPORT_BAD_PASSWORD_Name, NET_ERROR_PKCS12_IMPORT_BAD_PASSWORD_Des);
    AddErrorData(NET_ERROR_PKCS12_IMPORT_FAILED_ID, NET_ERROR_PKCS12_IMPORT_FAILED_Name, NET_ERROR_PKCS12_IMPORT_FAILED_Des);
    AddErrorData(NET_ERROR_IMPORT_CA_CERT_NOT_CA_ID, NET_ERROR_IMPORT_CA_CERT_NOT_CA_Name, NET_ERROR_IMPORT_CA_CERT_NOT_CA_Des);
    AddErrorData(NET_ERROR_IMPORT_CERT_ALREADY_EXISTS_ID, NET_ERROR_IMPORT_CERT_ALREADY_EXISTS_Name, NET_ERROR_IMPORT_CERT_ALREADY_EXISTS_Des);
    AddErrorData(NET_ERROR_IMPORT_CA_CERT_FAILED_ID, NET_ERROR_IMPORT_CA_CERT_FAILED_Name, NET_ERROR_IMPORT_CA_CERT_FAILED_Des);
    AddErrorData(NET_ERROR_IMPORT_SERVER_CERT_FAILED_ID, NET_ERROR_IMPORT_SERVER_CERT_FAILED_Name, NET_ERROR_IMPORT_SERVER_CERT_FAILED_Des);
    AddErrorData(NET_ERROR_PKCS12_IMPORT_INVALID_MAC_ID, NET_ERROR_PKCS12_IMPORT_INVALID_MAC_Name, NET_ERROR_PKCS12_IMPORT_INVALID_MAC_Des);
    AddErrorData(NET_ERROR_PKCS12_IMPORT_INVALID_FILE_ID, NET_ERROR_PKCS12_IMPORT_INVALID_FILE_Name, NET_ERROR_PKCS12_IMPORT_INVALID_FILE_Des);
    AddErrorData(NET_ERROR_PKCS12_IMPORT_UNSUPPORTED_ID, NET_ERROR_PKCS12_IMPORT_UNSUPPORTED_Name, NET_ERROR_PKCS12_IMPORT_UNSUPPORTED_Des);
    AddErrorData(NET_ERROR_KEY_GENERATION_FAILED_ID, NET_ERROR_KEY_GENERATION_FAILED_Name, NET_ERROR_KEY_GENERATION_FAILED_Des);
    AddErrorData(NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_FAILED_ID, NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_FAILED_Name, NET_ERROR_ORIGIN_BOUND_CERT_GENERATION_FAILED_Des);
    AddErrorData(NET_ERROR_PRIVATE_KEY_EXPORT_FAILED_ID, NET_ERROR_PRIVATE_KEY_EXPORT_FAILED_Name, NET_ERROR_PRIVATE_KEY_EXPORT_FAILED_Des);
    AddErrorData(NET_ERROR_DNS_MALFORMED_RESPONSE_ID, NET_ERROR_DNS_MALFORMED_RESPONSE_Name, NET_ERROR_DNS_MALFORMED_RESPONSE_Des);
    AddErrorData(NET_ERROR_DNS_SERVER_REQUIRES_TCP_ID, NET_ERROR_DNS_SERVER_REQUIRES_TCP_Name, NET_ERROR_DNS_SERVER_REQUIRES_TCP_Des);
    AddErrorData(NET_ERROR_DNS_SERVER_FAILED_ID, NET_ERROR_DNS_SERVER_FAILED_Name, NET_ERROR_DNS_SERVER_FAILED_Des);
    AddErrorData(NET_ERROR_DNS_TIMED_OUT_ID, NET_ERROR_DNS_TIMED_OUT_Name, NET_ERROR_DNS_TIMED_OUT_Des);
    AddErrorData(NET_ERROR_DNS_CACHE_MISS_ID, NET_ERROR_DNS_CACHE_MISS_Name, NET_ERROR_DNS_CACHE_MISS_Des);
    AddErrorData(NET_ERROR_DNS_SEARCH_EMPTY_ID, NET_ERROR_DNS_SEARCH_EMPTY_Name, NET_ERROR_DNS_SEARCH_EMPTY_Des);
    AddErrorData(NET_ERROR_DNS_SORT_ERROR_ID, NET_ERROR_DNS_SORT_ERROR_Name, NET_ERROR_DNS_SORT_ERROR_Des);
  end;
end;

function TCefErrorManager.IsUserAborted(const aErrCode: integer): Boolean;
begin
  // An operation was aborted (due to user action).
  // NET_ERROR_ABORTED = -3;
  Result := aErrCode = -3;
end;

Initialization

CefErrorManager := TCefErrorManager.Create;

Finalization

CefErrorManager.Free;

end.
