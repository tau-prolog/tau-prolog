var pl = require( "../modules/core.js" );

var exampleProgram =
  // Products
  "item(id(1), name(bread))." +
  "item(id(2), name(water))." +
  "item(id(3), name(apple))." +
  // Shops
  "shop(id(1), name(tau), location(spain))." +
  "shop(id(2), name(swi), location(netherlands))." +
  // Stock
  "stock(item(1), shop(1), count(23), price(0.33))." +
  "stock(item(2), shop(1), count(17), price(0.25))." +
  "stock(item(2), shop(2), count(34), price(0.31))." +
  "stock(item(3), shop(2), count(15), price(0.45)).";

var exampleQuery =
  "item(id(ItemID), name(bread))," +
  "stock(item(ItemID), shop(ShopID), _, price(Price))," +
  "shop(id(ShopID), name(Shop), _).";

describe('core', () => {
  var session;

  beforeEach(function () {
    session = pl.create( 1000 );
  })

  describe('.create', function () {
    it('returns session class', function () {
      expect( session.constructor.name ).toEqual( 'Session' );
    });
  });

  describe('.consult', function () {
    it('returns true', function () {
      expect( session.consult(exampleProgram) ).toBe( true  );
    });

    it('adds rules to session', function () {
      expect( Object.keys(session.rules) ).toHaveLength( 0 );

      session.consult( exampleProgram );

      expect( Object.keys(session.rules) ).toEqual( ["item/2", "shop/3", "stock/4"]  );
    });
  });

  describe('.query', function () {
    it('returns true', function () {
      session.consult( exampleProgram );
      expect( session.query(exampleQuery) ).toBe( true  );
    });

    it('adds goal to session thread', function () {
      expect( session.thread.points ).toHaveLength( 0 );
      session.consult( exampleProgram );
      session.query(exampleQuery);
      expect( session.thread.points ).toHaveLength( 1 );
      expect( session.thread.points[0] ).toHaveProperty( 'goal' );
    });
  });

  describe('.answers', function () {
    var results = [];

    beforeEach(function () {
      session = pl.create( 1000 );
      session.consult( exampleProgram );
      session.query( exampleQuery );
      session.answers(function( answer ) { results.push(answer) });
    });

    it('returns an anonymous function', function () {
      expect( session.answers.constructor.name ).toEqual( 'Function'  );
    });

    describe('when results are present', function () {
      it('function evaluation returns a substitution', function () {
        expect( results[0].constructor.name ).toEqual( 'Substitution' );
      });
    });

    describe('when no more results are present', function () {
      it('function evaluation returns false', function () {
        expect( results[1] ).toBe( false );
      });
    });
  });

  describe('.format_answer', function () {
    var results = [];

    beforeEach(function () {
      session = pl.create( 1000 );

      session.consult( exampleProgram );
      session.query( exampleQuery );
      session.answers(function( answer ) {
        results.push( pl.format_answer(answer) )
      });
    });

    it('returns formatted substitution', function () {
      expect( results[0] ).toEqual( 'ItemID = 1, ShopID = 1, Price = 0.33, Shop = tau ;' );
    });

    it('returns formatted false boolean', function () {
      expect( results[1] ).toBe( 'false.' );
    });
  });
});
