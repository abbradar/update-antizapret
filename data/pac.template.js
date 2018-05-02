var ADDRESSES_MAP = {}
for (var i = 0; i < ADDRESSES.length; ++i) {
  ADDRESSES_MAP[ADDRESSES[i]] = null;
}
ADDRESSES = undefined;

function FindProxyForURL(url, host) {
  var ipAddr = dnsResolve(host);

  if (convert_addr(ipAddr) in ADDRESSES_MAP) {
    return PROXY;
  }

  for (var i = 0; i < SUBNETS.length; i++) {
    var subnet = SUBNETS[i];
    if (isInNet(ipAddr, subnet.ip, subnet.mask)) {
      return PROXY;
    }
  }

  return "DIRECT";
}
