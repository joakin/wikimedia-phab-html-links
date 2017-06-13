const http = require('http');
const url = require('url');
const request = require('request');

const phabApiKey = process.env.PHAB_API_KEY;

http
  .createServer((req, res) => {
    res.setHeader('Access-Control-Allow-Origin', '*');

    const uri = url.parse(req.url, true);
    if (uri.pathname === '/phabricator') {
      const ids = uri.query.ids && uri.query.ids.split('|');
      if (!ids || !ids.length) {
        res.statusCode = 500;
        return res.end('Error parsing ids');
      }

      getPhabTasks(ids).pipe(res);
    } else if (uri.pathname === '/gerrit') {
      if (uri.query.id || uri.query.changeId) {
        getGerritPatch(uri.query).pipe(res);
      } else {
        res.statusCode = 500;
        return res.end('Error parsing id or changeId');
      }
    } else {
      res.statusCode = 404;
      res.end('Not found');
    }
  })
  .listen(process.env.PORT || 3000);

function getPhabTasks(ids) {
  const uri = 'https://phabricator.wikimedia.org/api/maniphest.search';
  return request.post(uri, {
    form: { 'api.token': phabApiKey, constraints: { ids } }
  });
}

function getGerritPatch({ id, changeId }) {
  const uri =
    'https://gerrit.wikimedia.org/r/changes/' +
    (id || '?q=' + encodeURIComponent(changeId));
  return request(uri).on('response', res => {
    delete res.headers['content-disposition'];
  });
}
