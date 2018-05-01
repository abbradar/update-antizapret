function FindProxyForURL(url, host) {
  var ipAddr = dnsResolve(host);

  if (ipAddr in ADDRESSES)
  {
    return PROXY;
  }

  for (var i = 0; i < SUBNETS.length; i++)
  {
    var subnet = SUBNETS[i];
    if (isInNet(ipAddr, subnet.ip, subnet.mask)) {
      return PROXY;
    }
  }

  return "DIRECT";
}
