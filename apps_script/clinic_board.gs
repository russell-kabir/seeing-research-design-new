/**
 * Research clinic · 研究相談室 — shared lab board for the Seeing Research Design app.
 *
 * The app runs in each student's browser (Shinylive), so posts are stored in a Google Sheet
 * through this small Apps Script web app.
 *
 * HOW TO SET IT UP (about 5 minutes)
 *   1. Go to script.google.com → New project → paste this whole file → Save.
 *   2. Choose the function "setup" → Run → approve the permissions.
 *      This creates a Google Sheet called "Research clinic board" in your Drive.
 *      The Logs show its URL.
 *   3. Deploy → New deployment → type "Web app"
 *        Execute as: Me      Who has access: Anyone
 *      → Deploy → copy the Web app URL (it ends in /exec).
 *   4. Paste that URL into CLINIC_URL at the top of app/clinic.R and push to GitHub.
 *      The site rebuilds itself; the "Lab board" step now shares posts across students.
 *
 * Each browser gets an anonymous id, so a student who posts again replaces their own row.
 * Run clearPosts() to empty the board before a new class or semester.
 */

const BOARD_CODE = 'kabirlab';        // must match CLINIC_CODE in app/clinic.R
const SHEET_NAME = 'Posts';
const HEADERS = ['updated', 'client', 'name', 'level', 'stage', 'title', 'design', 'data',
                 'ratings', 'question', 'tried', 'explore'];
const MAX_CHARS = 600;                // per field, as a safety net

function setup() {
  const ss = SpreadsheetApp.create('Research clinic board · 研究相談室');
  const sh = ss.getSheets()[0];
  sh.setName(SHEET_NAME);
  sh.getRange(1, 1, 1, HEADERS.length).setValues([HEADERS]).setFontWeight('bold');
  sh.setFrozenRows(1);
  PropertiesService.getScriptProperties().setProperty('SHEET_ID', ss.getId());
  Logger.log('Sheet created: ' + ss.getUrl());
  Logger.log('Now deploy this project as a Web app (Execute as: Me, Who has access: Anyone).');
}

function sheet_() {
  const id = PropertiesService.getScriptProperties().getProperty('SHEET_ID');
  if (!id) throw new Error('Run setup() first.');
  return SpreadsheetApp.openById(id).getSheetByName(SHEET_NAME);
}

function json_(obj) {
  return ContentService.createTextOutput(JSON.stringify(obj)).setMimeType(ContentService.MimeType.JSON);
}

function clean_(v) {
  return String(v == null ? '' : v).replace(/^[=+\-@]/, "'$&").slice(0, MAX_CHARS);
}

// GET ?action=list&code=…  → { ok: true, rows: [ {updated, name, …}, … ] }
function doGet(e) {
  const p = (e && e.parameter) || {};
  if (p.code !== BOARD_CODE) return json_({ ok: false, error: 'wrong code' });
  if (p.action !== 'list') return json_({ ok: false, error: 'unknown action' });
  const sh = sheet_();
  const values = sh.getDataRange().getValues();
  const head = values.shift() || HEADERS;
  const rows = values.filter((r) => r.join('') !== '').map((r) => {
    const o = {};
    head.forEach((h, i) => { if (h !== 'client') o[h] = r[i] instanceof Date ? Utilities.formatDate(r[i], Session.getScriptTimeZone(), 'yyyy-MM-dd HH:mm') : String(r[i]); });
    return o;
  });
  return json_({ ok: true, rows: rows });
}

// POST body (text/plain JSON): { action: "post", code, client, row: {…} }
function doPost(e) {
  let body;
  try { body = JSON.parse(e.postData.contents); } catch (err) { return json_({ ok: false, error: 'bad JSON' }); }
  if (body.code !== BOARD_CODE) return json_({ ok: false, error: 'wrong code' });
  if (body.action !== 'post' || !body.row) return json_({ ok: false, error: 'unknown action' });
  const lock = LockService.getScriptLock();
  lock.waitLock(10000);
  try {
    const sh = sheet_();
    const row = body.row;
    row.client = String(body.client || '').slice(0, 64);
    row.updated = new Date();
    const out = HEADERS.map((h) => (h === 'updated' ? row.updated : clean_(row[h])));
    const last = sh.getLastRow();
    let target = -1;
    if (row.client && last > 1) {
      const ids = sh.getRange(2, 2, last - 1, 1).getValues();
      for (let i = 0; i < ids.length; i++) if (String(ids[i][0]) === row.client) { target = i + 2; break; }
    }
    if (target > 0) sh.getRange(target, 1, 1, HEADERS.length).setValues([out]);
    else sh.appendRow(out);
    return json_({ ok: true });
  } finally {
    lock.releaseLock();
  }
}

function clearPosts() {
  const sh = sheet_();
  if (sh.getLastRow() > 1) sh.getRange(2, 1, sh.getLastRow() - 1, HEADERS.length).clearContent();
}
