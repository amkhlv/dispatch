# Seminar scheduler and tracker

This is a __web application__  to facilitate seminar scheduling (avoiding schedule conflicts) 
and tracking.
Page shows the list of scheduled seminars and events,
including periodic events (_e.g._ classes and regular seminars).

See [deployed example](https://andreimikhailov.com/seminars/sp/string/list)

Authorized users can login and add their own events.
Authorization is via Google.
Administrator keeps a list of authorized Gmail accounts.
New users can only be added by contacting administrator.
(This makes it only useful for relatively small groups of people, such as local research community.)

## Haskell Setup

[install Stack](https://haskell-lang.org/get-started)

## Building

	stack build

## Configuration

### Two configuration files

See sample files `common.xml` and `instance.xml`

### Javascript file

The directory specified as `<dir>...</dir>` in `instance.xml` should contain a file called `dispatch.js`.
This file may be empty.
Its content is placed as `<script>` in the generated page. This is a "hook" to allow further customization.

A sample `dispatch.js` is [included](dispatch.js)

### Certificates preparation

    openssl genrsa -out key.pem 2048
    openssl req -new -key key.pem -out certificate.csr

-- will ask questions ^^^

    openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem

### Setting up Google OAuth2.0

See [explanations on developers.google.com](https://developers.google.com/identity/protocols/oauth2).
Then, Client ID and Secret should be put in `common.xml`

## Running server

    stack exec dispatch -- -c common.xml -i instance.xml

### NGINX

    location /seminars/sp/string {
            rewrite /seminars/sp/string(.*) $1  break;
            proxy_pass https://localhost:11111;
    }
