= RESTful Bitcoin payment channel protocol


[[_overview]]
== Overview
A Bitcoin payment channel enables instant, secure transfer of bitcoins from one party to another. The intended use is consumer-to-merchant payments, allowing a consumer to make many small payments to a merchant – with whom the consumer has an open payment channel – while only paying the Bitcoin transaction fee once, when the channel is settled.

The payment server speaking this protocol is the recipient of value, operating on behalf of a content delivery server who receives value from a customer/client.

The content delivery server passes requests supplied by the client (payer) to the payment server, and delivers content to the client dependent on the response from the payment server.


=== Version information
[%hardbreaks]
_Version_ : 0.1.0


=== URI scheme
[%hardbreaks]
_Host_ : localhost:8000
_Schemes_ : HTTP




[[_paths]]
== Paths

[[_createpaymentchannel]]
=== Create a new payment channel
....
POST /channels/new
....


==== Description
After publishing the Bitcoin transaction which pays to the channel funding address, and waiting for the specified number of confirmations, the client will POST a payment paying the server's specified channel open price to the channel open URL, which is the URL returned by &lt;&lt;_getfundinginfo,fundingInfo&gt;&gt; plus the 'change_address' parameter.


==== Parameters

[options="header", cols=".^1,.^3,.^10,.^4,.^2"]
|===
|Type|Name|Description|Schema|Default
|*Query*|*change_address* +
_required_|Client/value sender change address. When the channel is closed, any value not transferred to the server over the channel is returned to this address.|string|
|*Query*|*client_pubkey* +
_required_|Client/value sender public key. Hex-encoded, compressed Secp256k1 pubkey, 33 bytes.|string|
|*Query*|*exp_time* +
_required_|The expiration date/time for the channel (Unix timestamp). After this point in time the channel refund transaction becomes valid, allowing the client to reclaim the channel funds in case the server goes missing.|integer(int64)|
|*Body*|*payment* +
_required_|Channel payment. New channel payment transferring additional value to the server|<<_payment,Payment>>|
|===


==== Responses

[options="header", cols=".^1,.^15,.^4"]
|===
|HTTP Code|Description|Schema
|*201*|Channel created|<<_chanopenresult,ChanOpenResult>>
|*400*|Invalid input (see header for error description)|No Content
|===


==== Consumes

* `application/json`


==== Produces

* `application/json`


[[_paypaymentchannel]]
=== Make payment over an existing payment channel
....
PUT /channels/{funding_txid}/{funding_vout}
....


==== Description
Example URL: http://pay.example.com/channels/f4cfa4a18d474d7e7c7d18edbb5c9c37293eea1136d2a01788342211c5e3d3f0/3


==== Parameters

[options="header", cols=".^1,.^3,.^10,.^4,.^2"]
|===
|Type|Name|Description|Schema|Default
|*Path*|*funding_txid* +
_required_|Transaction ID of the transaction which pays to the channel funding address.|string|
|*Path*|*funding_vout* +
_required_|Output index/vout of the output in the transaction paying to the channel funding address.|string|
|*Query*|*change_address* +
_optional_|New client/value sender change address (optional). If the client wishes to update its change address, it will set this parameter to the new change address and provide a Payment with the specified change address.|string|
|*Body*|*payment* +
_required_|Channel payment. New channel payment transferring additional value to the server|<<_payment,Payment>>|
|===


==== Responses

[options="header", cols=".^1,.^15,.^4"]
|===
|HTTP Code|Description|Schema
|*200*|Payment received|<<_paymentresult,PaymentResult>>
|*400*|Invalid input (see header for error description)|No Content
|*404*|No such payment channel|No Content
|===


==== Consumes

* `application/json`


==== Produces

* `application/json`


[[_deletepaymentchannel]]
=== Delete an existing payment channel
....
DELETE /channels/{funding_txid}/{funding_vout}
....


==== Description
Example URL: http://pay.example.com/channels/f4cfa4a18d474d7e7c7d18edbb5c9c37293eea1136d2a01788342211c5e3d3f0/3


==== Parameters

[options="header", cols=".^1,.^3,.^10,.^4,.^2"]
|===
|Type|Name|Description|Schema|Default
|*Path*|*funding_txid* +
_required_|Transaction ID of the transaction which pays to the channel funding address.|string|
|*Path*|*funding_vout* +
_required_|Output index/vout of the output in the transaction paying to the channel funding address.|string|
|*Body*|*payment* +
_required_|Most recent channel payment. The most recent payment previously sent to the server is included here as a form of authentication token, proving the client is party to the payment channel in question.|<<_payment,Payment>>|
|===


==== Responses

[options="header", cols=".^1,.^15,.^4"]
|===
|HTTP Code|Description|Schema
|*200*|Channel closed, funds settled. The settling transaction has been published to the Bitcoin network.|No Content
|*400*|Invalid input (see header for error description)|No Content
|*404*|No such payment channel|No Content
|===


==== Consumes

* `application/json`


==== Produces

* `application/json`


[[_getfundinginfo]]
=== Retrieve information about how to fund a new payment channel.
....
GET /fundingInfo
....


==== Description
Before opening a payment channel with the server, the client must first acquire the server public key, in order to calculate a funding address for the channel.

The client will first calculate the funding address, confirm that it matches the server's, then pay to the funding address, and wait until the funding transaction has the server-specified number of confirmations.

After this, the client will create a new payment of value equal to the server-specified channel open price, and POST this to the channel-open URL (see &lt;&lt;_createpaymentchannel,here&gt;&gt;), in order to open the channel.


==== Parameters

[options="header", cols=".^1,.^3,.^10,.^4,.^2"]
|===
|Type|Name|Description|Schema|Default
|*Query*|*client_pubkey* +
_required_|Client/value sender public key. Hex-encoded, compressed Secp256k1 pubkey, 33 bytes.|string|
|*Query*|*exp_time* +
_required_|The expiration date/time for the channel (Unix timestamp). After this point in time the channel refund transaction becomes valid, allowing the client to reclaim the channel funds in case the server goes missing.|integer(int64)|
|===


==== Responses

[options="header", cols=".^1,.^15,.^4"]
|===
|HTTP Code|Description|Schema
|*200*|Proceed with funding|<<_fundinginfo,FundingInfo>>
|*400*|Invalid input (see header for error description)|No Content
|===




[[_definitions]]
== Definitions

[[_chanopenresult]]
=== ChanOpenResult

[options="header", cols=".^3,.^11,.^4"]
|===
|Name|Description|Schema
|*channel_uri* +
_required_|The URL of the newly opened channel. Payments are PUT on this URL.|string
|*payment_result* +
_required_||<<_paymentresult,PaymentResult>>
|===


[[_fundinginfo]]
=== FundingInfo

[options="header", cols=".^3,.^11,.^4"]
|===
|Name|Description|Schema
|*channel_open_uri* +
_required_|The URL which must be POSTed to in order to open a new payment channel. See &lt;&lt;_createpaymentchannel,here&gt;&gt;.|string
|*funding_address* +
_required_|Payment channel funding address. Send bitcoins to this address to fund a new channel. +
*Example* : `"2NCTirSGjFM8T7hUow3AcfyFaw1N1APnYuP"`|string
|*funding_tx_min_conf* +
_required_|Minimum confirmation count that the funding transaction must have before proceeding with opening a new channel.|integer(int32)
|*open_price* +
_required_|Price (in satoshis) for opening a channel with the given {exp_time}. This amount is paid in the initial channel payment when creating a new channel. May be zero, in which case a payment of zero value is transferred, ensuring that the channel can be closed at any time. +
*Example* : `25000`|integer(int64)
|*server_pubkey* +
_required_|Server/value receiver public key. Hex-encoded, compressed Secp256k1 pubkey, 33 bytes. +
*Example* : `"029b5549e8cac42d27051956925d8176408b2183ba357850f58320ad5876b9c13f\""`|string
|===


[[_payment]]
=== Payment

[options="header", cols=".^3,.^11,.^4"]
|===
|Name|Description|Schema
|*payment_data* +
_required_|Opaque payment data, base64-encoded|string
|===


[[_paymentresult]]
=== PaymentResult

[options="header", cols=".^3,.^11,.^4"]
|===
|Name|Description|Schema
|*channel_value_left* +
_required_|Remaining channel value. This is the amount that the client/sender would receive if the channel was closed now.|integer(int64)
|*value_received* +
_required_|Value of the payment that was just received. This is the additional value assigned to the receiver/server with this payment.|integer(int64)
|===






