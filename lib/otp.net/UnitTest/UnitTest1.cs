using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Otp;

namespace Otp
{
    /// <summary>
    /// Summary description for UnitTest1
    /// </summary>
    [TestClass]
    public class UnitTest1
    {
        public UnitTest1()
        {
            //
            // TODO: Add constructor logic here
            //
        }

        private TestContext testContextInstance;

        /// <summary>
        ///Gets or sets the test context which provides
        ///information about and functionality for the current test run.
        ///</summary>
        public TestContext TestContext
        {
            get
            {
                return testContextInstance;
            }
            set
            {
                testContextInstance = value;
            }
        }

        #region Additional test attributes
        //
        // You can use the following additional attributes as you write your tests:
        //
        // Use ClassInitialize to run code before running the first test in the class
        // [ClassInitialize()]
        // public static void MyClassInitialize(TestContext testContext) { }
        //
        // Use ClassCleanup to run code after all tests in a class have run
        // [ClassCleanup()]
        // public static void MyClassCleanup() { }
        //
        // Use TestInitialize to run code before running each test 
        // [TestInitialize()]
        // public void MyTestInitialize() { }
        //
        // Use TestCleanup to run code after each test has run
        // [TestCleanup()]
        // public void MyTestCleanup() { }
        //
        #endregion

        [TestMethod]
        public void TestFormat()
        {
            {
                Erlang.Object obj1 = Erlang.Object.Format("a");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.Atom));
                Assert.AreEqual("a", (obj1 as Erlang.Atom).atomValue());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("$a");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.Char));
                Assert.AreEqual('a', (obj1 as Erlang.Char).charValue());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("'Abc'");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.Atom));
                Assert.AreEqual("Abc", (obj1 as Erlang.Atom).atomValue());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("\"Abc\"");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.String));
                Assert.AreEqual("Abc", (obj1 as Erlang.String).stringValue());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("Abc");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.Var));
                Assert.AreEqual("Abc", (obj1 as Erlang.Var).name());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("1");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.Long));
                Assert.AreEqual(1, (obj1 as Erlang.Long).longValue());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("1.23");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.Double));
                Assert.AreEqual(1.23, (obj1 as Erlang.Double).doubleValue());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("V");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.Var));
                Assert.AreEqual("V", (obj1 as Erlang.Var).name());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("{1}");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.Tuple));
                Assert.AreEqual(1, (obj1 as Erlang.Tuple).arity());
                Assert.IsInstanceOfType((obj1 as Erlang.Tuple)[0], typeof(Erlang.Long));
                Assert.AreEqual(1, ((obj1 as Erlang.Tuple)[0] as Erlang.Long).longValue());
            }
            {
                Erlang.Object obj0 = Erlang.Object.Format("[]");
                Assert.IsInstanceOfType(obj0, typeof(Erlang.List));
                Assert.AreEqual(0, (obj0 as Erlang.List).arity());
                Erlang.Object obj1 = Erlang.Object.Format("[1]");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.List));
                Assert.AreEqual(1, (obj1 as Erlang.List).arity());
                Assert.IsInstanceOfType((obj1 as Erlang.List)[0], typeof(Erlang.Long));
                Assert.AreEqual(1, ((obj1 as Erlang.List)[0] as Erlang.Long).longValue());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("[{1,2}, []]");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.List));
                Assert.AreEqual(2, (obj1 as Erlang.List).arity());
                Assert.IsInstanceOfType((obj1 as Erlang.List)[0], typeof(Erlang.Tuple));
                Assert.AreEqual(2, ((obj1 as Erlang.List)[0] as Erlang.Tuple).arity());
                Assert.AreEqual(0, ((obj1 as Erlang.List)[1] as Erlang.List).arity());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("{a, [b, 1, 2.0, \"abc\"], {1, 2}}");
                Assert.IsInstanceOfType(obj1, typeof(Erlang.Tuple));
                Assert.AreEqual(3, (obj1 as Erlang.Tuple).arity());
            }
            {
                Erlang.Object obj1 = Erlang.Object.Format("~w", 1);
                Assert.IsInstanceOfType(obj1, typeof(Erlang.Long));
                Assert.AreEqual(1, (obj1 as Erlang.Long).longValue());
                Erlang.Object obj2 = Erlang.Object.Format("{~w, ~w,~w}", 1, 2, 3);
                Assert.IsInstanceOfType(obj2, typeof(Erlang.Tuple));
                Assert.AreEqual(3, (obj2 as Erlang.Tuple).arity());
                Assert.IsInstanceOfType((obj2 as Erlang.Tuple)[0], typeof(Erlang.Long));
                Assert.AreEqual(1, ((obj2 as Erlang.Tuple)[0] as Erlang.Long).longValue());
                Assert.IsInstanceOfType((obj2 as Erlang.Tuple)[1], typeof(Erlang.Long));
                Assert.AreEqual(2, ((obj2 as Erlang.Tuple)[1] as Erlang.Long).longValue());
                Assert.IsInstanceOfType((obj2 as Erlang.Tuple)[2], typeof(Erlang.Long));
                Assert.AreEqual(3, ((obj2 as Erlang.Tuple)[2] as Erlang.Long).longValue());
            }
            {
                Erlang.Object obj2 = Erlang.Object.Format("{~w, ~w,~w,~w, ~w}", 1.0, 'a', "abc", 2, true);
                Assert.IsInstanceOfType(obj2, typeof(Erlang.Tuple));
                Assert.AreEqual(5, (obj2 as Erlang.Tuple).arity());
                Assert.IsInstanceOfType((obj2 as Erlang.Tuple)[0], typeof(Erlang.Double));
                Assert.AreEqual(1.0, ((obj2 as Erlang.Tuple)[0] as Erlang.Double).doubleValue());
                Assert.IsInstanceOfType((obj2 as Erlang.Tuple)[1], typeof(Erlang.Char));
                Assert.AreEqual('a', ((obj2 as Erlang.Tuple)[1] as Erlang.Char).charValue());
                Assert.IsInstanceOfType((obj2 as Erlang.Tuple)[2], typeof(Erlang.String));
                Assert.AreEqual("abc", ((obj2 as Erlang.Tuple)[2] as Erlang.String).stringValue());
                Assert.IsInstanceOfType((obj2 as Erlang.Tuple)[3], typeof(Erlang.Long));
                Assert.AreEqual(2, ((obj2 as Erlang.Tuple)[3] as Erlang.Long).longValue());
                Assert.IsInstanceOfType((obj2 as Erlang.Tuple)[4], typeof(Erlang.Boolean));
                Assert.AreEqual(true, ((obj2 as Erlang.Tuple)[4] as Erlang.Boolean).booleanValue());
            }
        }
        [TestMethod]
        public void TestPatternMatch()
        {
            {
                Erlang.Object pattern = Erlang.Object.Format("{test, A, B, C}");
                Erlang.Object obj = Erlang.Object.Format("{test, 10, a, [1,2,3]}");

                Erlang.VarBind binding = new Otp.Erlang.VarBind();
                Assert.IsTrue(pattern.match(obj, binding));
                Assert.AreEqual(3, binding.Count);
                Assert.AreEqual(10, binding["A"].longValue());
                Assert.AreEqual("a", binding["B"].atomValue());
                Assert.AreEqual("[1,2,3]", binding["C"].ToString());
            }
        }
    }
}
