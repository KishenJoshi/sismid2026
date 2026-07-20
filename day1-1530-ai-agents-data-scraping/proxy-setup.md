# Proxy / VPN setup for live Google Trends during teaching

> **Status: built and verified (2026-07-17).** A spare Ubuntu-on-WSL box on a residential
> Comcast connection runs **tinyproxy** (auth `sismid` / password in the box's
> `~/proxy/proxy.env`), exposed publicly with **ngrok** (`tcp://…ngrok.io:PORT`). A real
> GitHub Codespace routed through it showed egress `a residential IP` (Comcast, **not** Azure)
> and completed a full Google Trends pull (92 weekly points). Students point at it with one
> line: `export HTTPS_PROXY=http://sismid:PASS@HOST:PORT`. ngrok's free-plan TCP address is
> **ephemeral** (changes on restart); re-read it with
> `curl -s localhost:4040/api/tunnels | grep -oE 'tcp://[^"]+'`. Note: ngrok TCP requires a
> (free, uncharged) card on the account for ID verification.

## Why

Google Trends throttles requests by **source IP**. GitHub Codespaces (and Colab) egress
from **Azure/Google datacenter IPs**. In practice this is milder than expected: a real
Codespace test hit one HTTP 429 but **succeeded on retry**, so a proxy is usually *not*
required. Treat it as a **backup** for the cases where retries keep failing, for example a
whole room bursting at once, or a venue whose IP really is blocked. The classic fix: send
the request through a machine with an **ordinary residential IP**. You have a spare Linux
box or Mac; turn it into that machine.

Verify for your own network with `course-repo/scripts/gt_smoke_test.py`: it prints the
egress IP/org and whether a live pull succeeds. Run it once with no proxy (a Codespace will
likely 429 then recover on retry) and once with the proxy (expect your home IP + success).

Two approaches, both covered below:

| | Proxy server (tinyproxy) | VPN exit node (Tailscale) |
|---|---|---|
| Scope | per-app (`HTTPS_PROXY`) | whole machine |
| Reaching a home box behind NAT | needs a tunnel (Tailscale/ngrok) or port-forward | built in (Tailscale mesh) |
| Best when | you only need Google Trends to go through | you want everything to route home, cleanly |
| Security | must add a password; never leave it open | private mesh, nothing exposed |

**Recommendation.** For machines *you* control (your laptop, a Codespace you launched),
the Tailscale exit node (B) is cleanest. For onboarding a whole **room** of student
devices, the HTTP proxy (A) is usually *less* hassle: students set one environment
variable and never join your tailnet. A Tailscale exit node only works for devices that
are members of your tailnet, so putting 30 student laptops on your personal (Gmail)
tailnet means onboarding each one (see B3) and sending all their traffic through your home
uplink. Simplest of all: run the pull *on the home machine itself* and screen-share (no
client setup).

---

## Approach A: HTTP proxy with tinyproxy

`tinyproxy` is a tiny HTTP/HTTPS forward proxy. Same tool on Linux and Mac.

### A1. Install and configure on the home machine

**Linux (Ubuntu/Debian):**
```bash
sudo apt update && sudo apt install -y tinyproxy
sudo sed -i 's/^Port .*/Port 8888/' /etc/tinyproxy/tinyproxy.conf
# allow your LAN and (if using Tailscale) the tailnet; require a password:
echo 'Allow 100.64.0.0/10'          | sudo tee -a /etc/tinyproxy/tinyproxy.conf   # Tailscale range
echo 'BasicAuth sismid CHANGE_ME'   | sudo tee -a /etc/tinyproxy/tinyproxy.conf
sudo systemctl restart tinyproxy && sudo systemctl enable tinyproxy
```

**macOS (Homebrew):**
```bash
brew install tinyproxy
CONF="$(brew --prefix)/etc/tinyproxy/tinyproxy.conf"
sed -i '' 's/^Port .*/Port 8888/' "$CONF"
printf 'Allow 100.64.0.0/10\nBasicAuth sismid CHANGE_ME\n' >> "$CONF"
brew services start tinyproxy
```

Pick a real password instead of `CHANGE_ME`. `Allow` lines control who may connect; keep
them tight. With `BasicAuth` set, clients must send `user:pass`.

### A2. Make the home proxy reachable (it is behind NAT)

Choose one:

- **Tailscale (recommended, private).** Install Tailscale on the home box and on each
  client (see Approach B1). The proxy is then reachable at the home box's tailnet IP,
  e.g. `100.101.102.103:8888`. Nothing is exposed to the public internet.
- **ngrok (quick, public).** `ngrok tcp 8888` prints something like
  `tcp://7.tcp.ngrok.io:19112`. Use that host:port. Keep `BasicAuth` on, because this is
  now reachable by anyone who learns the URL.
- **Router port-forward (last resort).** Forward external `8888` to the home box, use your
  public IP, and rely on `BasicAuth` + a firewall allowlist. Least safe; avoid if you can.

### A3. Point the pull at the proxy (client side)

Environment variable (works for `requests`, `curl`, most tools):
```bash
export HTTPS_PROXY="http://sismid:CHANGE_ME@HOST:8888"
export HTTP_PROXY="http://sismid:CHANGE_ME@HOST:8888"
```

Or pass it straight to pytrends:
```python
from pytrends.request import TrendReq
proxies = ["http://sismid:CHANGE_ME@HOST:8888"]   # http:// proxy scheme, even for https traffic
pt = TrendReq(hl="en-US", tz=360, retries=3, backoff_factor=0.5, proxies=proxies)
```

Quick test from a client:
```bash
curl -x "http://sismid:CHANGE_ME@HOST:8888" -s -o /dev/null -w "%{http_code}\n" \
  "https://trends.google.com/?geo=MX"
```
A `200`/`301` means the proxy path works; then run `gt_smoke_test.py` with `HTTPS_PROXY`
set and confirm the egress IP is now your home IP.

---

## Approach B: VPN exit node with Tailscale

A Tailscale **exit node** routes *all* of a client's traffic out through the home machine.
No open ports, no password to leak; clients are authenticated into your private tailnet.

### B1. Home machine as the exit node

**Linux:**
```bash
curl -fsSL https://tailscale.com/install.sh | sh
# enable IP forwarding so it can route others' traffic:
printf 'net.ipv4.ip_forward = 1\nnet.ipv6.conf.all.forwarding = 1\n' \
  | sudo tee /etc/sysctl.d/99-tailscale.conf
sudo sysctl -p /etc/sysctl.d/99-tailscale.conf
sudo tailscale up --advertise-exit-node
```

**macOS:** install the Tailscale app (Mac App Store or tailscale.com/download), sign in,
then in a terminal:
```bash
/Applications/Tailscale.app/Contents/MacOS/Tailscale up --advertise-exit-node
```

Then, in the **Tailscale admin console** (login.tailscale.com), open the home machine and
**approve it as an exit node** (Machines -> the box -> Edit route settings -> Use as exit
node). This one-time approval is required.

### B2. Clients

- **A laptop:** install Tailscale, sign in, and pick the exit node (app menu -> Exit node
  -> the home box), or:
  ```bash
  sudo tailscale up --exit-node=HOME_BOX_NAME --exit-node-allow-lan-access
  ```
  All traffic, including Google Trends, now exits from the home residential IP.

- **A Codespace** (no `/dev/net/tun`, so use userspace mode):
  ```bash
  curl -fsSL https://tailscale.com/install.sh | sh
  sudo tailscaled --tun=userspace-networking \
       --outbound-http-proxy-listen=localhost:1055 \
       --socks5-server=localhost:1055 &
  sudo tailscale up --exit-node=HOME_BOX_NAME --auth-key=tskey-auth-XXXX
  export HTTPS_PROXY=http://localhost:1055 HTTP_PROXY=http://localhost:1055
  ```
  Generate a short-lived, reusable `--auth-key` in the admin console (Settings -> Keys)
  so students do not each need to log in interactively.

---

## Letting a whole room use it

A Tailscale exit node only works for devices **on your tailnet**. Your tailnet is tied to
your Google login, so to use it each student device has to join it. Two honest paths:

**Easier for a room: the HTTP proxy (Approach A), no tailnet needed.**
- Students on the **same classroom wifi** point at the teaching machine's LAN IP:
  `export HTTPS_PROXY=http://sismid:PASS@<LAN-IP>:8888`. No tunnel, nothing public.
- Students on **Codespaces** cannot reach a LAN IP from the cloud, so expose the proxy
  with a tunnel (ngrok/Tailscale) and hand out that URL instead.

**If you do want the room on your Tailnet (auth keys):**
1. Admin console (login.tailscale.com) -> **Settings -> Keys -> Generate auth key**.
   Turn on **Reusable** and **Ephemeral**, give it a short expiry and a tag (`tag:sismid`).
2. Each student runs, once:
   ```bash
   sudo tailscale up --auth-key=tskey-auth-XXXX \
        --exit-node=HOME_BOX_NAME --exit-node-allow-lan-access
   ```
   Auth-key nodes attach as **devices under your account** (not new users), so you stay
   within the free plan's 1-user / 100-device limits; ephemeral devices auto-remove when
   they disconnect.
3. Restrict them with an ACL so student nodes can only reach the internet via the exit
   node, not each other or your other machines:
   ```json
   {
     "tagOwners": { "tag:sismid": ["autogroup:admin"] },
     "acls": [
       { "action": "accept", "src": ["tag:sismid"],        "dst": ["autogroup:internet:*"] },
       { "action": "accept", "src": ["autogroup:member"],  "dst": ["*:*"] }
     ]
   }
   ```
4. After class: revoke the auth key (Settings -> Keys), and the ephemeral nodes disappear.

**Reality check on "the classroom wifi IP".** If students are on their own laptops on the
classroom wifi, they *already* egress from the classroom's public IP; routing them through
a second machine on the same wifi does not change their IP. The proxy/VPN only changes the
IP for clients that would otherwise egress elsewhere, mainly **Codespaces** (Azure). And a
shared campus IP is not guaranteed to be un-throttled: verify with `gt_smoke_test.py`
through the proxy before relying on it. The one IP we have already confirmed works is an
ordinary **home residential** connection.

## Security and teardown

- **Never run an unauthenticated proxy on a public IP.** Use `BasicAuth` (tinyproxy) or
  keep it private (Tailscale). Rotate the password; do not commit it to the repo.
- A proxy/exit node sees your traffic metadata. Only route what you need, from machines
  you trust.
- **Tear down after class:** `brew services stop tinyproxy` / `sudo systemctl stop
  tinyproxy`, kill the ngrok tunnel, and `sudo tailscale down` (or disable the exit node).
- Auth keys are secrets. Make them short-lived and revoke them after the session.

## The simplest option (if the above is too much)

Run the live pull **on the home machine itself** (SSH in, or just sit at it) and
screen-share the result. Its residential IP works, no client proxy config is needed, and
students follow along on the cache. Use the proxy/VPN only when you want the *room* to
pull live.
